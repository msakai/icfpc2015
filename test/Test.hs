{-# LANGUAGE
    TemplateHaskell
 #-}
module Main where

import Control.Monad
import Data.Maybe
import Data.List.Split
import qualified Data.Set as Set
import qualified System.FilePath.Glob as Glob
import Text.Printf

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
  , Game.gsPhrases   = []
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

computeScore :: Game.Input -> Int -> String -> Int
computeScore input seedNo solution = Game.gsScore $ Game.gameStepN cmds gs0
  where
    gs0 = Game.initGameStates input phrases !! seedNo
    cmds = stringToCommands solution

checkScore :: FilePath -> Int -> [String] -> IO ()
checkScore problem ub tags = do
  Just input  <- Util.readProblem problem
  ss <- forM tags $ \tag -> do
    let (_:s:_) = splitOn "-" tag
        idx = read $ drop 4 s
    [fname] <- Glob.glob ("outputs/*pt-" ++ tag ++ ".json")
    Just [o] <- Util.readOutput fname
    return $ computeScore input idx (Game.solution o)
  assertBool "invalid number of tags" (length ss <= length (Game.sourceSeeds input))
  let s = sum ss `div` length (Game.sourceSeeds input)
  assertBool (printf "average score = %d, but it should be <=%d" s ub) (s <= ub)
  return ()

case_problem_0_score = checkScore "problems/problem_0.json" ub tags
  where
    ub = 3209 -- from http://icfpcontest.org/leader_board.html
    tags = ["problem0-seed0-14-6-14.999508000000"]

case_problem_1_score = checkScore "problems/problem_1.json" ub tags
  where
    ub = 2276 -- from http://icfpcontest.org/leader_board.html
    tags = ["problem1-seed0-5-12-40.045950000000"]

case_problem_2_score = checkScore "problems/problem_2.json" ub tags
  where
    ub = 4637 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem2-seed0-7-56-0.908282000000"
      , "problem2-seed1-7-57-21.659918000000"
      , "problem2-seed2-7-58-30.026467000000"
      , "problem2-seed3-7-59-44.019384000000"
      , "problem2-seed4-8-0-58.746652000000"
      , "problem2-seed5-3-11-8.063302000000"
      , "problem2-seed6-3-15-3.934693000000"
      , "problem2-seed7-2-30-9.989391000000"
      , "problem2-seed8-3-22-40.707238000000"
      , "problem2-seed9-3-26-59.895375000000"
      ]

case_problem_3_score = checkScore "problems/problem_3.json" ub tags
  where
    ub = 1586 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem3-seed0-15-2-13.385372000000"
      , "problem3-seed1-15-3-14.099391000000"
      , "problem3-seed2-5-35-48.382411000000"
      , "problem3-seed3-5-35-50.653841000000"
      , "problem3-seed4-15-5-38.242267000000"
      ]

case_problem_4_score = checkScore "problems/problem_4.json" ub tags
  where
    ub = 2311 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem4-seed37-5-34-41.959404000000"
      , "problem4-seed15-3-10-54.434211000000"
      , "problem4-seed0-2-53-59.223337000000"
      , "problem4-seed8-3-3-4.978209000000"
      , "problem4-seed9-3-4-20.722952000000"
      , "problem4-seed42-3-42-56.559699000000"
      , "problem4-seed11-5-32-29.069316000000"
      , "problem4-seed46-17-30-41.435291000000"
      , "problem4-seed49-3-50-54.347004000000"
      , "problem4-seed47-3-48-27.613848000000"
      , "problem4-seed39-3-39-9.223707000000"
      , "problem4-seed6-5-35-56.293000000000"
      , "problem4-seed40-3-40-44.752048000000"
      , "problem4-seed33-5-34-20.995562000000"
      , "problem4-seed18-5-33-5.962029000000"
      , "problem4-seed19-3-15-15.072037000000"
      , "problem4-seed34-16-54-8.309790000000"
      , "problem4-seed38-3-37-21.189779000000"
      , "problem4-seed12-15-53-15.761036000000"
      , "problem4-seed41-17-17-26.680527000000"
      , "problem4-seed7-3-1-25.680815000000"
      , "problem4-seed36-3-34-51.333872000000"
      , "problem4-seed14-15-59-40.380758000000"
      , "problem4-seed21-5-33-18.923337000000"
      , "problem4-seed27-16-35-6.182637000000"
      , "problem4-seed29-5-34-4.216769000000"
      , "problem4-seed31-16-45-47.060487000000"
      , "problem4-seed16-3-12-9.150634000000"
      , "problem4-seed44-3-45-2.963473000000"
      , "problem4-seed25-3-22-5.942966000000"
      , "problem4-seed43-3-43-59.839641000000"
      , "problem4-seed48-3-49-55.053875000000"
      , "problem4-seed13-3-9-4.699206000000"
      , "problem4-seed26-5-33-45.489656000000"
      , "problem4-seed5-2-59-27.942948000000"
      , "problem4-seed28-3-25-25.427934000000"
      , "problem4-seed23-3-19-55.334061000000"
      , "problem4-seed45-5-40-48.549947000000"
      , "problem4-seed35-5-34-33.655194000000"
      , "problem4-seed3-2-57-26.696233000000"
      , "problem4-seed24-16-28-5.239949000000"
      , "problem4-seed2-2-56-35.487333000000"
      , "problem4-seed4-2-58-27.924803000000"
      , "problem4-seed30-3-27-41.692865000000"
      , "problem4-seed22-3-18-36.854959000000"
      , "problem4-seed20-3-16-29.933924000000"
      , "problem4-seed1-2-55-19.110534000000"
      , "problem4-seed10-3-6-3.739061000000"
      , "problem4-seed32-3-29-49.297832000000"
      , "problem4-seed17-5-33-1.191057000000"
      ]

case_problem_5_score = checkScore "problems/problem_5.json" ub tags
  where
    ub = 1572 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem5-seed0-2-53-31.369952000000"
      , "problem5-seed6-2-54-4.820116000000"
      , "problem5-seed5-5-31-25.476215000000"
      , "problem5-seed3-5-31-18.939834000000"
      , "problem5-seed9-5-31-37.288580000000"
      , "problem5-seed4-15-8-23.166417000000"
      , "problem5-seed2-2-53-40.747500000000"
      , "problem5-seed8-5-31-34.772529000000"
      , "problem5-seed7-2-54-10.382729000000"
      ]

case_problem_6_score = checkScore "problems/problem_6.json" ub tags
  where
    ub = 2065 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem6-seed25-5-29-14.436024000000"
      , "problem6-seed20-5-28-49.029859000000"
      , "problem6-seed32-5-29-44.584307000000"
      , "problem6-seed12-5-28-16.599601000000"
      , "problem6-seed45-5-30-41.782942000000"
      , "problem6-seed35-5-29-56.591358000000"
      , "problem6-seed23-5-29-4.336895000000"
      , "problem6-seed17-5-28-35.335526000000"
      , "problem6-seed38-5-30-12.202847000000"
      , "problem6-seed11-5-28-11.752235000000"
      , "problem6-seed27-5-29-23.141033000000"
      , "problem6-seed4-5-27-39.708994000000"
      , "problem6-seed49-5-31-4.400156000000"
      , "problem6-seed10-5-28-5.917246000000"
      , "problem6-seed43-5-30-32.334252000000"
      , "problem6-seed24-5-29-9.182643000000"
      , "problem6-seed13-5-28-20.529653000000"
      , "problem6-seed21-5-28-54.034333000000"
      , "problem6-seed1-2-8-49.361525000000"
      , "problem6-seed18-5-28-40.695670000000"
      , "problem6-seed3-5-27-35.000834000000"
      , "problem6-seed37-5-30-6.017525000000"
      , "problem6-seed34-5-29-53.235476000000"
      , "problem6-seed31-5-29-39.647921000000"
      , "problem6-seed30-5-29-35.407731000000"
      , "problem6-seed26-5-29-18.703629000000"
      , "problem6-seed36-5-30-1.317942000000"
      , "problem6-seed42-5-30-28.314247000000"
      , "problem6-seed15-5-28-26.357192000000"
      , "problem6-seed2-2-11-18.791355000000"
      , "problem6-seed28-5-29-27.188327000000"
      , "problem6-seed44-5-30-37.220475000000"
      , "problem6-seed47-5-30-54.948530000000"
      , "problem6-seed8-5-27-56.917215000000"
      , "problem6-seed6-5-27-48.620266000000"
      , "problem6-seed22-5-28-58.150260000000"
      , "problem6-seed7-5-27-52.475510000000"
      , "problem6-seed39-5-30-15.181856000000"
      , "problem6-seed19-5-28-44.573047000000"
      , "problem6-seed9-5-28-1.134289000000"
      , "problem6-seed14-5-28-23.519413000000"
      , "problem6-seed46-5-30-47.009225000000"
      , "problem6-seed33-5-29-48.801020000000"
      , "problem6-seed16-5-28-30.340640000000"
      , "problem6-seed5-5-27-44.083439000000"
      , "problem6-seed48-5-31-0.060074000000"
      , "problem6-seed29-5-29-31.295408000000"
      , "problem6-seed40-5-30-19.327596000000"
      , "problem6-seed0-5-27-20.728939000000"
      , "problem6-seed41-5-30-23.519821000000"
      ]

case_problem_7_score = checkScore "problems/problem_7.json" ub tags
  where
    ub = 1708 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem7-seed1-5-27-3.239631000000"
      , "problem7-seed2-15-16-56.547204000000"
      , "problem7-seed4-15-17-41.783258000000"
      , "problem7-seed3-15-17-16.981345000000"
      , "problem7-seed0-15-16-19.156624000000"
      ]

case_problem_8_score = checkScore "problems/problem_8.json" ub tags
  where
    ub = 2048 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem8-seed3-2-21-15.898951000000"
      , "problem8-seed8-2-56-16.771123000000"
      , "problem8-seed0-15-19-1.609609000000"
      , "problem8-seed2-15-19-21.292616000000"
      , "problem8-seed6-2-56-5.643508000000"
      , "problem8-seed5-2-55-59.031476000000"
      , "problem8-seed9-2-56-26.334845000000"
      , "problem8-seed4-2-55-54.378073000000"
      , "problem8-seed1-5-26-24.081135000000"
      , "problem8-seed7-2-21-42.185441000000"
      ]

case_problem_9_score = checkScore "problems/problem_9.json" ub tags
  where
    ub = 836 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem9-seed1-15-53-11.315530000000"
      , "problem9-seed4-15-52-56.994058000000"
      , "problem9-seed3-2-21-25.444377000000"
      , "problem9-seed2-2-21-18.213047000000"
      , "problem9-seed0-2-21-55.147434000000"
      ]

case_problem_10_score = checkScore "problems/problem_10.json" ub tags
  where
    ub = 334 -- from http://icfpcontest.org/leader_board.html
    tags = ["problem10-seed0-15-31-0.506624000000"]

case_problem_11_score = checkScore "problems/problem_11.json" ub tags
  where
    ub = 2096 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem11-seed1-15-54-57.182474000000"
      , "problem11-seed0-15-54-9.950248000000"
      , "problem11-seed4-15-56-20.832038000000"
      , "problem11-seed3-15-31-22.864323000000"
      , "problem11-seed2-5-26-12.976893000000"
      ]

case_problem_12_score = checkScore "problems/problem_12.json" ub tags
  where
    ub = 2962 -- from http://icfpcontest.org/leader_board.html
    tags =
      [ "problem12-seed5-3-0-30.109344000000"
      , "problem12-seed0-2-56-38.591987000000"
      , "problem12-seed3-15-39-57.246224000000"
      , "problem12-seed4-2-59-53.996994000000"
      , "problem12-seed9-3-3-47.120247000000"
      , "problem12-seed8-2-30-34.879970000000"
      , "problem12-seed6-15-43-27.510722000000"
      , "problem12-seed1-2-57-26.663117000000"
      , "problem12-seed2-15-38-56.140716000000"
      , "problem12-seed7-5-24-53.443679000000"
      ]

case_problem_13_score = checkScore "problems/problem_13.json" ub tags
  where
    ub = 1959 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem13-seed0-2-52-33.774497000000" ]

case_problem_14_score = checkScore "problems/problem_14.json" ub tags
  where
    ub = 1462 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem14-seed0-18-35-12.602379000000" ]

case_problem_15_score = checkScore "problems/problem_15.json" ub tags
  where
    ub = 2744 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem15-seed0-18-37-20.368250000000" ]

case_problem_16_score = checkScore "problems/problem_16.json" ub tags
  where
    ub = 1976 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem16-seed0-18-40-41.368008000000" ]

case_problem_17_score = checkScore "problems/problem_17.json" ub tags
  where
    ub = 3336 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem17-seed0-18-47-25.151224000000" ]

case_problem_18_score = checkScore "problems/problem_18.json" ub tags
  where
    ub = 1964 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem18-seed0-19-21-16.767622000000" ]

case_problem_19_score = checkScore "problems/problem_19.json" ub tags
  where
    ub = 3374 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem19-seed0-19-29-50.304074000000" ]

case_problem_20_score = checkScore "problems/problem_20.json" ub tags
  where
    ub = 3802 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem20-seed0-20-22-10.449738000000" ]

case_problem_21_score = checkScore "problems/problem_21.json" ub tags
  where
    ub = 1464 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem21-seed0-19-47-7.963364000000" ]

case_problem_22_score = checkScore "problems/problem_22.json" ub tags
  where
    ub = 2600 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem22-seed0-7-7-18.623260000000" ]

case_problem_23_score = checkScore "problems/problem_23.json" ub tags
  where
    ub = 639 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem23-seed0-20-2-11.029939000000" ]

case_problem_24_score = checkScore "problems/problem_24.json" ub tags
  where
    ub = 2536 -- from http://icfpcontest.org/leader_board.html
    tags = [ "problem24-seed0-20-3-43.370629000000" ]

main :: IO ()
main = $(defaultMainGenerator)
