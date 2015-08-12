{-# LANGUAGE
    TemplateHaskell
 #-}
module Main where

import Data.Maybe
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Test.QuickCheck.Monadic as QM

import Types
import qualified Board    
import Command
import qualified Cell
import qualified Game
import qualified PRNG
import qualified Unit
import qualified Util

instance Arbitrary FaceDir where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MDir where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CDir where
  arbitrary = arbitraryBoundedEnum

arbitraryCell :: Gen Cell.Cell
arbitraryCell = do
  x <- choose (- 1024, 1024)
  y <- choose (- 1024, 1024)
  return $ Cell.Cell x y

prop_moveCell_inverse =
  forAll arbitraryCell $ \c ->
    forAll arbitrary $ \dir ->
      Cell.moveCell (oppositeFaceDir dir) (Cell.moveCell dir c) == c

prop_moveCell_commute =
  forAll arbitraryCell $ \c ->
    forAll arbitrary $ \dir1 ->
      forAll arbitrary $ \dir2 ->
        Cell.moveCell dir2 (Cell.moveCell dir1 c) == Cell.moveCell dir1 (Cell.moveCell dir2 c)

case_moveCell_1 = Cell.moveCell FaceSE (Cell.Cell 1 1) @?= Cell.Cell 2 2
case_moveCell_2 = Cell.moveCell FaceSW (Cell.Cell 1 1) @?= Cell.Cell 1 2
case_moveCell_3 = Cell.moveCell FaceNE (Cell.Cell 1 1) @?= Cell.Cell 2 0
case_moveCell_4 = Cell.moveCell FaceNW (Cell.Cell 1 1) @?= Cell.Cell 1 0
case_moveCell_5 = Cell.moveCell FaceSE (Cell.Cell 1 2) @?= Cell.Cell 1 3
case_moveCell_6 = Cell.moveCell FaceSW (Cell.Cell 1 2) @?= Cell.Cell 0 3
case_moveCell_7 = Cell.moveCell FaceNE (Cell.Cell 1 2) @?= Cell.Cell 1 1
case_moveCell_8 = Cell.moveCell FaceNW (Cell.Cell 1 2) @?= Cell.Cell 0 1

prop_turnCell_inverse_1 =
  forAll arbitraryCell $ \c ->
    forAll arbitraryCell $ \p ->
        Cell.turnCell CCW p (Cell.turnCell CW p c) == c

prop_turnCell_inverse_2 =
  forAll arbitraryCell $ \c ->
    forAll arbitraryCell $ \p ->
        Cell.turnCell CCW p (Cell.turnCell CW p c) == c

prop_turnCell_commute =
  forAll arbitraryCell $ \c ->
    forAll arbitraryCell $ \p ->
      forAll arbitrary $ \cdir1 ->
        forAll arbitrary $ \cdir2 ->
          Cell.turnCell cdir1 p (Cell.turnCell cdir2 p c) == Cell.turnCell cdir2 p (Cell.turnCell cdir1 p c)

prop_turnCell_pivot_fixed =
  forAll arbitraryCell $ \c ->
    forAll arbitrary $ \cdir ->
      Cell.turnCell cdir c c == c

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


exampleUnit1e = Unit.Unit{ Unit.members = Set.fromList [Cell.Cell 1 4, Cell.Cell 2 4, Cell.Cell 2 5], Unit.pivot = Cell.Cell 1 4 }
exampleUnit1o = Unit.Unit{ Unit.members = Set.fromList [Cell.Cell 1 3, Cell.Cell 2 3, Cell.Cell 3 4], Unit.pivot = Cell.Cell 1 3 }

case_spawnUnit_ExampleUnit1e = Unit.spawn (5,10) exampleUnit1e @?= Unit.Unit{ Unit.members = Set.fromList [Cell.Cell 1 0, Cell.Cell 2 0, Cell.Cell 2 1], Unit.pivot = Cell.Cell 1 0 }
case_spawnUnit_ExampleUnit1o = Unit.spawn (5,10) exampleUnit1o @?= Unit.Unit{ Unit.members = Set.fromList [Cell.Cell 1 0, Cell.Cell 2 0, Cell.Cell 2 1], Unit.pivot = Cell.Cell 1 0 }


case_gameStep_status_1 = Game.gsStatus (Game.gameStep (Move SW) exampleGameState) @?= Game.Finished

case_gameStep_status_2 = Game.gsStatus (Game.gameStep (Turn CW) exampleGameState) @?= Game.Error

exampleGameState
  = Game.GameState
  { Game.gsProblemId = 0
  , Game.gsSeed      = 0
  , Game.gsUnits     = []
  , Game.gsBoard     = Board.Board{ Board.cols = 2, Board.rows = 2, Board.fulls = Set.fromList [Cell.Cell 1 0, Cell.Cell 0 1, Cell.Cell 1 1] }
  , Game.gsCurUnit   = u
  , Game.gsSource    = []
  , Game.gsLocked    = False
  , Game.gsStatus    = Game.Running
  , Game.gsCommands  = []
  , Game.gsTrace     = Set.singleton u
  , Game.gsLs        = 0
  , Game.gsScore     = 0
  , Game.gsPOccur    = Set.empty
  }
  where
    u = Unit.Unit{ Unit.members = Set.singleton (Cell.Cell 0 0), Unit.pivot = Cell.Cell 0 0 }

main :: IO ()
main = $(defaultMainGenerator)
