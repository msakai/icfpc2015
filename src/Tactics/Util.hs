{-# LANGUAGE
    BangPatterns
  #-}
module Tactics.Util
  ( allLockablePlaces
  , randomWalkTillLocked
  , randomWalkTillLockedWithPPs
  ) where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified System.Random as Rand

import Command
import qualified Game
import Unit (Unit)
import qualified Unit
import Util (readProblem)

-- 状態を受け取って、ロック可能な位置を列挙して返す
-- Mapの値は、ロックまでのコマンド列とロック後の状態の1つ
-- 同じ位置の複数の状態がありえるが、それは枝刈りしてしまう
allLockablePlaces :: Game.GameState -> Map Unit ([Command], Game.GameState)
allLockablePlaces g | Game.gsStatus g /= Game.Running = Map.empty
allLockablePlaces g = loop (Map.singleton (Game.gsCurUnit g) ([], g)) Set.empty Map.empty
  where
    loop :: Map Unit ([Command], Game.GameState) -> Set Unit -> Map Unit ([Command], Game.GameState) -> Map Unit ([Command], Game.GameState)
    loop !q !visited !locked
      | Map.null q = Map.map (\(cmds, g) -> (reverse cmds, g)) locked
      | otherwise  = loop q' visited' locked'
          where
            visited' = visited `Set.union` Map.keysSet q
            locked'  = locked `Map.union` Map.fromList [x | x@(u2, (cmds,g2)) <- xs, Game.gsLocked g2]
            q' = Map.fromList [x | x@(u2, (cmds,g2)) <- xs, not (Game.gsLocked g2), u2 `Set.notMember` visited]     
            xs = [ (u2, (c : cmds, g2))
                 | (u, (cmds,g)) <- Map.toList q
                 , Game.gsStatus g == Game.Running
                 , c <- allCommands
                 , let g2 = Game.gameStep c g
                 , Game.gsStatus g2 /= Game.Error
                 , let u2 = applyCommand c u -- Game.gsCurUnit g2 は次のピースの可能性があるので別途計算
                 , u2 `Set.notMember` visited
                 ]

randomWalkTillLocked :: Rand.RandomGen g => g -> Game.GameState -> ([Command], Game.GameState, g)
randomWalkTillLocked = randomWalkTillLockedWithPPs []

randomWalkTillLockedWithPPs :: Rand.RandomGen g => [[Command]] -> g -> Game.GameState -> ([Command], Game.GameState, g)                        
randomWalkTillLockedWithPPs pp g gs = loop g [] gs
  where
    loop g acts gs
      | Game.gsLocked gs2 = (concat (reverse acts2), gs2, g')
      | otherwise = loop g' acts2 gs2
      where
        (acts2, gs2) =
          if null gss2 then error "randomWalkTillLockWithPP: should not happen"
          else gss2 !! i
        --gss2 = [(act : acts, gs2) | act <- allActions, let gs2 = Game.gameStepN act gs, Game.gsStatus gs2 /= Game.Error]
        --gss2 = [(act2 : acts, gs2) | act <- allActions, let (act2,gs2) = stepN act gs, Game.gsStatus gs2 /= Game.Error]
        gss2 = [(act2 : acts, gs2) | act <- allActions, (act2,gs2) <- stepN2 act gs, Game.gsStatus gs2 /= Game.Error]
        (i, g') = Rand.randomR (0, length gss2 - 1) g

    allActions = [[c] | c <- allCommands] ++ pp

stepN :: [Command] -> Game.GameState -> ([Command], Game.GameState)
stepN (c:cs) = loop cs [c] . Game.gameStep c
  where
    loop [] hist g = (reverse hist, g)
    loop _ hist g | Game.gsLocked g = (reverse hist, g)
    loop (c:cs) hist g = loop cs (c : hist) (Game.gameStep c g)

stepN2 :: [Command] -> Game.GameState -> [([Command], Game.GameState)]
stepN2 (c:cs) = loop cs [c] . Game.gameStep c
  where
    loop [] hist g = [(reverse hist, g)]
    loop _ hist g | Game.gsLocked g = [] -- (reverse hist, g)
    loop (c:cs) hist g = loop cs (c : hist) (Game.gameStep c g)                         

applyCommand :: Command -> Unit -> Unit
applyCommand (Move dir) = Unit.move dir
applyCommand (Turn dir) = Unit.turn dir

test_allLockablePlaces = do
  Just input <- readProblem "problems/problem_1.json"
  let gs0 = head $ Game.initGameStates input []
      m = allLockablePlaces gs0
  Game.gameDisplay gs0
  forM_ (Map.toList m) $ \(u, (cmds, g)) -> do
    putStrLn "----"
    Game.gameDisplay g

test_randomWalkTillLocked = do
  Just input <- readProblem "problems/problem_1.json"
  let gs0 = head $ Game.initGameStates input []
  Game.gameDisplay gs0
  g <- Rand.newStdGen
  putStrLn "----"
  let (cmds, gs, g') = randomWalkTillLocked g gs0
  -- Game.gameDisplay gs
  Game.gameDisplay $ Game.gameStepN (init cmds) gs0 -- ロックして次のピースが出てくる前の状態を印字
  print cmds
