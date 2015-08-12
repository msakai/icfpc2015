{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
 #-}
module Game
  ( Input (..)
  , OutputItem (..)
  , Output

  , GameStatus (..)
  , GameState (..)
  , initGameStates
  , gameStep
  , gameStepN

  , Game
  , gameDisplay
  , gameDisplay'
  , dumpOutputItem
  ) where

import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.Array as Arr
import qualified Data.Set as Set
import Data.List
import Data.Ix (inRange)
import GHC.Generics
import Prelude hiding (id)

import qualified Text.PrettyPrint.Boxes as PPr

import Board
import Types
import Cell
import Display
import Unit
import Command
import PRNG
import Score

data Input = Input { id :: Number
                   , units :: [Unit]
                   , width :: Number
                   , height :: Number
                   , filled :: [Cell]
                   , sourceLength :: Number
                   , sourceSeeds :: [Number]
                   }
             deriving (Show, Eq, Generic)

instance FromJSON Input
instance ToJSON Input

data OutputItem = OutputItem { problemId :: Number
                             , seed :: Number
                             , tag :: String
                             , solution :: String
                             }
                deriving (Show, Eq, Generic)

type Output = [OutputItem]

instance FromJSON OutputItem
instance ToJSON OutputItem
instance FromJSON Output
instance ToJSON Output

initSources :: Input -> [(Number,[Unit])]
initSources input = zip seeds 
                  $ map (initSource arr . take (sourceLength input) . generate . fromIntegral) seeds
  where
    seeds = sourceSeeds input
    us  = units input
    len = length us
    arr = Arr.listArray (0,len-1) us

initSource :: Arr.Array Int Unit -> [Int] -> [Unit]
initSource arr rs = map ((arr Arr.!) . (`mod` m)) rs
  where
    m     = h + 1
    (l,h) = Arr.bounds arr

data GameStatus = Running | Finished | Error deriving (Show, Eq)

data GameState = GameState 
  { gsProblemId :: Number
  , gsSeed      :: Number
  , gsUnits     :: [Unit]
  --
  , gsBoard     :: Board
  , gsCurUnit   :: Unit
  , gsSource    :: [Unit]
  --
  , gsLocked    :: Bool
  , gsStatus    :: GameStatus
  , gsCommands  :: Commands
  , gsTrace     :: Set.Set Unit
  --
  , gsLs        :: Pt
  , gsScore     :: Pt
  , gsPOccur    :: Set.Set Commands
  }
  deriving (Show)

initGameStates :: Input -> [GameState]
initGameStates input = map (initGameState (defaultGameState input)) (initSources input)
  where
    defaultGameState :: Input -> GameState
    defaultGameState input = GameState
      { gsProblemId = id input
      , gsSeed      = undefined
      , gsUnits     = units input
      -- 
      , gsBoard     = mkBoard (width input) (height input) (filled input)
      , gsCurUnit   = undefined
      , gsSource    = undefined
      --
      , gsLocked    = False
      , gsStatus    = Running
      , gsCommands  = []
      , gsTrace     = Set.empty
      --
      , gsLs        = 0
      , gsScore     = 0
      , gsPOccur    = Set.empty
      }

    initGameState :: GameState -> (Number,[Unit]) -> GameState
    initGameState d (sd,src) = d { gsSeed    = sd
                                 , gsCurUnit = iu
                                 , gsSource  = tail src
                                 , gsTrace   = Set.singleton iu
                                 }
      where
        iu = spawn (cols (gsBoard d), rows (gsBoard d)) (head src)

gameStepN :: [Command] -> GameState -> GameState
gameStepN cmds old = foldl' (flip gameStep) old cmds

gameStep :: Command -> GameState -> GameState
gameStep _ old | gsStatus old /= Running = old
gameStep cmd old = old { gsBoard     = newboard
                       , gsCurUnit   = case newstatus of
                           Game.Error    -> oldcur
                           Game.Finished -> oldcur
                           _             -> if lockedp then freshcur else newcur
                       , gsSource    = if lockedp then tail oldsource else oldsource
                       , gsLocked    = lockedp
                       , gsStatus    = newstatus
                       , gsCommands  = newcmds
                       , gsTrace     = if lockedp then Set.singleton freshcur
                                       else Set.insert newcur oldtrace
                       , gsLs        = ls
                       , gsScore     = newscore
                       , gsPOccur    = newpoccur
                       }
  where
    nospace   = not (isValidUnit newboard freshcur)
    fini      = null oldsource
    freshcur  = spawn (cols oldboard, rows oldboard) (head oldsource)
    oldsource = gsSource old
    oldtrace  = gsTrace old
    oldcur    = gsCurUnit old
    newcur    = issue cmd oldcur
    oldboard  = gsBoard old
    newboard  = if not lockedp then oldboard else newb
    (ls,newb) = clearFullRows (lockUnit oldboard oldcur)
    lockedp   = not (isValidUnit oldboard newcur)
    oldscore  = gsScore old
    old_ls    = gsLs old
    unitsize  = if lockedp then size oldcur else 0
    newscore  = case newstatus of
      Game.Error -> 0
      _          -> oldscore + move_score unitsize ls old_ls + pwrscr
    newstatus = if Set.member newcur oldtrace then Game.Error
                else if lockedp && (fini || nospace) then Game.Finished
                     else Game.Running
    newcmds   = cmd : gsCommands old
    oldpoccur = gsPOccur old
    (pwrscr,newpoccur) = power_score newcmds oldpoccur

issue :: Command -> Unit -> Unit
issue (Move dir) = move dir
issue (Turn dir) = turn dir

type Game = StateT GameState IO

gameDisplay :: GameState -> IO ()
gameDisplay gm = PPr.printBox $ dispBoard (gsBoard gm) [gsCurUnit gm]

gameDisplay' :: GameState -> IO ()
gameDisplay' gm = gameDisplay gm >> putStrLn ("Score : " ++ show (gsScore gm))

dumpOutputItem :: GameState -> String -> OutputItem
dumpOutputItem gm tag = OutputItem
  { problemId = gsProblemId gm
  , seed      = gsSeed gm
  , tag       = tag
  , solution  = head $ commandsToString $ reverse (gsCommands gm)
  }
