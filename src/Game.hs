{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
 #-}
module Game where

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

initBoard :: Input -> Board
initBoard i = Board { cols = width i, rows = height i, fulls = Set.fromList (filled i) }

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
  , gsTag       :: String
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
  }

defaultGameState :: String -> Input -> GameState
defaultGameState tg input = GameState
  { gsProblemId = id input
  , gsSeed      = undefined
  , gsTag       = tg
  , gsUnits     = units input
  -- 
  , gsBoard     = initBoard input
  , gsCurUnit   = undefined
  , gsSource    = undefined
  --
  , gsLocked    = False
  , gsStatus    = Running
  , gsCommands  = []
  , gsTrace     = Set.empty
  }

initGameStates :: String -> Input -> [GameState]
initGameStates tg input = map (initGameState (defaultGameState tg input)) (initSources input)

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
gameStep cmd old = old { gsBoard     = newboard
                       , gsCurUnit   = if lockedp then if fini || nospace then oldcur 
                                                       else fleshcur
                                       else newcur
                       , gsSource    = if lockedp then tail oldsource else oldsource
                       , gsLocked    = lockedp
                       , gsStatus    = if Set.member newcur oldtrace then Game.Error
                                       else if (lockedp && fini) || nospace then Game.Finished
                                       else Game.Running
                       , gsCommands  = cmd : gsCommands old
                       , gsTrace     = if lockedp then Set.singleton fleshcur
                                       else Set.insert newcur oldtrace
                       }
  where
    nospace   = not (valid newboard fleshcur)
    fini      = null oldsource
    fleshcur  = spawn (cols oldboard, rows oldboard) (head oldsource)
    oldsource = gsSource old
    oldtrace  = gsTrace old
    oldcur    = gsCurUnit old
    newcur    = issue cmd oldcur
    oldboard  = gsBoard old
    newboard  = if not lockedp then oldboard 
                else clearFullRows (lockUnit oldboard oldcur)
    lockedp   = not (valid oldboard newcur)

issue :: Command -> Unit -> Unit
issue (Move dir) = move dir
issue (Turn dir) = turn dir

type Game = StateT GameState IO

game :: Game ()
game = undefined

gameDisplay :: GameState -> IO ()
gameDisplay gm = PPr.printBox $ dispBoard (gsBoard gm) [gsCurUnit gm]

dumpOutputItem :: GameState -> OutputItem
dumpOutputItem gm = OutputItem
  { problemId = gsProblemId gm
  , seed      = gsSeed gm
  , tag       = gsTag gm
  , solution  = head $ commandsToString $ reverse (gsCommands gm)
  }

