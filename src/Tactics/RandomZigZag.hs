module Tactics.RandomZigZag where

import System.Random

import Types
import Command

player :: IO [Command]
player = do
  g <- newStdGen
  return $ map convert (randoms g)
  where
    convert :: Int -> Command
    convert n = if odd n then Move SE else Move SW
