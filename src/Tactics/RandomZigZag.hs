module Tactics.RandomZigZag where

import System.Random

import Types
import Command
import Game (GameState)
import Play

newPlayer :: IO Player
newPlayer = do
  g <- newStdGen
  return $ replayPlayer $ map convert (randoms g)
  where
    convert :: Int -> Command
    convert n = if odd n then Move SE else Move SW
