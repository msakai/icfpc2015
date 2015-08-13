module Tactics.Hybrid
       ( newPlayer
       )where

import Control.Arrow ((&&&))
import qualified Data.Set as Set
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
import Tactics.Util (randomWalkTillLockedWithPPs, allLockablePlaces)
import Tactics.Reynolds (evalGS)

newPlayer :: Int -> IO Player
newPlayer n = do
  rs <- sequence $ take n $ repeat Rand.newStdGen
  return $ player rs

player :: Rand.RandomGen r => [r] -> Player
player rs = do
  gs0 <- getGameState
  let pps = [cmds | pp <- Game.gsPhrases gs0, let cmds = stringToCommands pp]
  let loop rs = do
        gs <- getGameState
        let xs = [randomWalkTillLockedWithPPs pps r gs | r <- rs]
            rs' = [r' | (cmds,gs',r') <- xs]
            futures = [(cmds,gs') | (cmds,gs',r') <- xs] ++ Map.elems (allLockablePlaces gs)
        case filter (\(cmds,gs') -> Game.gsLs gs' > 0) futures of
          [] -> do
            let (cmds,gs') = maximumBy (comparing (\(cmds,gs') -> evalGS gs')) futures
            mapM_ command cmds
          (cmds,gs') : _ -> do
            mapM_ command cmds
        loop rs'
  loop rs
