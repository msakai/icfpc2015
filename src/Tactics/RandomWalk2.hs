module Tactics.RandomWalk2
  ( newPlayer
  ) where

import qualified Data.Set as Set
import qualified System.Random as Rand

import Types
import Command
import qualified Game
import qualified Unit
import Play

newPlayer :: IO Player
newPlayer = do
  g <- Rand.newStdGen
  return $ player g

player :: Rand.RandomGen g => g -> Player
player g = do
  gs <- getGameState
  let u = Game.gsCurUnit gs
      commands = [c | c <- allCommands, applyCommand c u `Set.notMember` Game.gsTrace gs]
  if null commands then
    command (head commands)
  else do
    let (i, g') = Rand.randomR (0, length commands - 1) g
    command $ commands !! i
    player g'

applyCommand :: Command -> Unit.Unit -> Unit.Unit
applyCommand (Move dir) = Unit.move dir
applyCommand (Turn dir) = Unit.turn dir
