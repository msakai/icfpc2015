{-# LANGUAGE
    TemplateHaskell
 #-}
module Main where

import Data.Maybe

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Test.QuickCheck.Monadic as QM

import Types
import qualified Cell
import qualified PRNG
import qualified Unit
import qualified Util

case_moveCell_1 = Cell.moveCell FaceSE (Cell.Cell 1 1) @?= Cell.Cell 2 2
case_moveCell_2 = Cell.moveCell FaceSW (Cell.Cell 1 1) @?= Cell.Cell 1 2
case_moveCell_3 = Cell.moveCell FaceNE (Cell.Cell 1 1) @?= Cell.Cell 2 0
case_moveCell_4 = Cell.moveCell FaceNW (Cell.Cell 1 1) @?= Cell.Cell 1 0
case_moveCell_5 = Cell.moveCell FaceSE (Cell.Cell 1 2) @?= Cell.Cell 1 3
case_moveCell_6 = Cell.moveCell FaceSW (Cell.Cell 1 2) @?= Cell.Cell 0 3
case_moveCell_7 = Cell.moveCell FaceNE (Cell.Cell 1 2) @?= Cell.Cell 1 1
case_moveCell_8 = Cell.moveCell FaceNW (Cell.Cell 1 2) @?= Cell.Cell 0 1

case_turnCell_1 = Cell.turnCell CW (Cell.Cell 2 2) (Cell.Cell 2 2) @?= Cell.Cell 2 2
case_turnCell_2 = Cell.turnCell CCW (Cell.Cell 2 2) (Cell.Cell 2 2) @?= Cell.Cell 2 2
case_turnCell_3 = Cell.turnCell CW (Cell.Cell 2 3) (Cell.Cell 2 3) @?= Cell.Cell 2 3
case_turnCell_4 = Cell.turnCell CCW (Cell.Cell 2 3) (Cell.Cell 2 3) @?= Cell.Cell 2 3
case_turnCell_5 = Cell.turnCell CW (Cell.Cell 2 2) (Cell.Cell 3 5) @?= Cell.Cell 0 5
case_turnCell_6 = Cell.turnCell CCW (Cell.Cell 2 2) (Cell.Cell 0 5) @?= Cell.Cell 3 5
case_turnCell_7 = Cell.turnCell CW (Cell.Cell 2 2) (Cell.Cell 4 4) @?= Cell.Cell 1 5
case_turnCell_8 = Cell.turnCell CCW (Cell.Cell 2 2) (Cell.Cell 1 5) @?= Cell.Cell 4 4
case_turnCell_9 = Cell.turnCell CW (Cell.Cell 2 3) (Cell.Cell 3 5) @?= Cell.Cell 1 5
case_turnCell_10 = Cell.turnCell CCW (Cell.Cell 2 3) (Cell.Cell 1 5) @?= Cell.Cell 3 5
case_turnCell_11 = Cell.turnCell CW (Cell.Cell 2 3) (Cell.Cell 4 4) @?= Cell.Cell 2 5
case_turnCell_12 = Cell.turnCell CCW (Cell.Cell 2 3) (Cell.Cell 2 5) @?= Cell.Cell 4 4

case_PRNG_generate =
  take 10 (PRNG.generate 17) @?= [0,24107,16552,12125,9427,13152,21440,3383,6873,16117]

case_readProblem = do
  r <- Util.readProblem "problems/problem_18.json"
  isJust r @? "failed to parse problem_18.json"


exampleUnit1e = Unit.Unit{ Unit.members = [Cell.Cell 1 4, Cell.Cell 2 4, Cell.Cell 2 5], Unit.pivot = Cell.Cell 1 4 }
exampleUnit1o = Unit.Unit{ Unit.members = [Cell.Cell 1 3, Cell.Cell 2 3, Cell.Cell 3 4], Unit.pivot = Cell.Cell 1 3 }

case_spawnUnit_ExampleUnit1e = Unit.spawn (5,10) exampleUnit1e @?= Unit.Unit{ Unit.members = [Cell.Cell 1 0, Cell.Cell 2 0, Cell.Cell 2 1], Unit.pivot = Cell.Cell 1 0 }
case_spawnUnit_ExampleUnit1o = Unit.spawn (5,10) exampleUnit1o @?= Unit.Unit{ Unit.members = [Cell.Cell 1 0, Cell.Cell 2 0, Cell.Cell 2 1], Unit.pivot = Cell.Cell 1 0 }

main :: IO ()
main = $(defaultMainGenerator)
