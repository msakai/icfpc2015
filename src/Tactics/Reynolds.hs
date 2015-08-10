module Tactics.Reynolds
       ( newPlayer
       )where

import Data.List
import Data.Ord (comparing)
import qualified System.Random as Rand

import Types
import Command
import qualified Game
import qualified Unit
import Play
import Tactics.Util (randomWalkTillLocked)

newPlayer :: IO Player
newPlayer = do
  rs <- sequence $ take 10 $ repeat Rand.newStdGen
  return $ player rs

player :: Rand.RandomGen r => [r] -> Player
player rs = do
  gs <- getGameState
  let futures = [randomWalkTillLocked r gs | r <- rs]
      (cmds,gs',r') = maximumBy (comparing (\(cmds,gs',r') -> evalGS gs')) futures
  mapM_ command cmds
  player [r' | (cmds,gs',r') <- futures]

evalGS :: Game.GameState -> Int
evalGS = Game.gsScore
