module Tactics.SearchAllLocking
       ( player
       )where

import Control.Arrow ((&&&))
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Ord (comparing)
import qualified System.Random as Rand

import Types
import qualified Board
import Cell
import Command
import qualified Game
import qualified Unit
import Play

import Tactics.Reynolds (evalGS)
import Tactics.Util (allLockablePlaces)

player :: Player
player = do
  gs <- getGameState
  let futures = Map.elems $ allLockablePlaces gs
      (cmds,gs') = maximumBy (comparing (\(cmds,gs') -> evalGS gs')) futures
  mapM_ command cmds
  player
