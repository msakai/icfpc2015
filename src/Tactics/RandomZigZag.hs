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
    convert n = case mod n 6 of
      0 -> Move E
      1 -> Move W
      2 -> Move SE
      3 -> Move SW
      4 -> Turn CW
      _ -> Turn CCW
