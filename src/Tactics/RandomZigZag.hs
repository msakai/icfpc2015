module Tactics.RandomZigZag where

import System.Random

import Types
import Command
import Game (GameState)

player :: GameState -> IO Commands
player _ = do
  g <- newStdGen
  return $ map convert (randoms g)
  where
    convert :: Int -> Command
    convert n = if odd n then Move SE else Move SW
