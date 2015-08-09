module Tactics.RandomZigZag where

import System.Random

import Types
import Command
import Game (GameState)
import Play

newPlayer :: IO IOPlayer
newPlayer = do
  g <- newStdGen
  mkReplayPlayer $ map convert (randoms g)
  where
    convert :: Int -> Command
    convert n = if odd n then Move SE else Move SW
