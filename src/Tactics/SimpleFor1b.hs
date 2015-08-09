module Tactics.SimpleFor1b
       ( player
       , vanishRow1
       ) where

import Types
import Command
import Game (GameState)
import Play

vanishRow1 :: [Command]
vanishRow1 = concat $
  [ xs ++ replicate i (Move E) ++ [Move E]
  | i <- [14,13..0]
  ]
  where
    xs = replicate 6 (Move W) ++ [Move SW] ++ concat (replicate 6 [Move SW, Move SE]) ++ [Move SW]

player :: Player
player = cycle vanishRow1
