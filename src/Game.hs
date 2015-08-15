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
  , gsScore
  , initGameState
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

initSource :: [Unit] -> Number -> Int -> [Unit]
initSource units seed n = map (\i -> arr Arr.! (i `mod` len)) $ take n $ generate $ fromIntegral seed
  where
    len = length units
    arr = Arr.listArray (0,len-1) units

data GameStatus = Running | Finished | Error deriving (Show, Eq)

data GameState = GameState 
  { gsProblemId :: Number
  , gsSeed      :: Number
  , gsPhrases   :: [String]
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
  , gsMScore    :: Pt
  , gsPScore    :: Pt
  }
  deriving (Show)

initGameState :: Input -> [String] -> Number -> GameState
initGameState input phrases seed = defaultGameState
  { gsSeed    = seed
  , gsCurUnit = iu
  , gsSource  = tail src
  , gsTrace   = Set.singleton iu
  }
  where
    src = initSource (units input) seed (sourceLength input)
    iu = spawn (cols (gsBoard defaultGameState), rows (gsBoard defaultGameState)) (head src)

    defaultGameState :: GameState
    defaultGameState = GameState
      { gsProblemId = id input
      , gsSeed      = undefined
      , gsPhrases   = phrases
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
      , gsMScore    = 0
      , gsPScore    = 0
      }
                 
initGameStates :: Input -> [String] -> [GameState]
initGameStates input phrases = [initGameState input phrases seed | seed <- sourceSeeds input]

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
                       , gsMScore    = newmscore
                       , gsPScore    = newpscore
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
    (ls,newboard) =
      if lockedp
      then clearFullRows (lockUnit oldboard oldcur)
      else (old_ls, oldboard)
    lockedp   = not (isValidUnit oldboard newcur)
    oldmscore = gsMScore old
    old_ls    = gsLs old
    newmscore  = case newstatus of
      Game.Error -> 0
      _ | lockedp -> oldmscore + move_score (size oldcur) ls old_ls
        | otherwise -> oldmscore
    newstatus = if Set.member newcur oldtrace then Game.Error
                else if lockedp && (fini || nospace) then Game.Finished
                     else Game.Running
    newcmds   = cmd : gsCommands old
    oldpscore = gsPScore old
    newpscore = power_score (gsPhrases old) newcmds

gsScore :: GameState -> Pt
gsScore g = gsMScore g + gsPScore g

issue :: Command -> Unit -> Unit
issue (Move dir) = move dir
issue (Turn dir) = turn dir

type Game = StateT GameState IO

gameDisplay :: GameState -> IO ()
gameDisplay gm = PPr.printBox $ dispBoard (gsBoard gm) [gsCurUnit gm]

gameDisplay' :: GameState -> IO ()
gameDisplay' gm = do
  gameDisplay gm
  putStrLn $ "Score : " ++ show (gsScore gm) ++ " (Move Score: " ++ show (gsMScore gm) ++ ", Power Score: " ++ show (gsPScore gm) ++ ")"

dumpOutputItem :: GameState -> String -> OutputItem
dumpOutputItem gm tag = OutputItem
  { problemId = gsProblemId gm
  , seed      = gsSeed gm
  , tag       = tag
  , solution  = head $ commandsToString $ reverse (gsCommands gm)
  }
