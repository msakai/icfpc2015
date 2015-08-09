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
      commands2 = [c | c <- commands, applyCommand c u `Set.notMember` Game.gsTrace gs]
  if null commands2 then
    command (head commands)
  else do
    let (i, g') = Rand.randomR (0, length commands2 - 1) g
    command $ commands2 !! i
    player g'

commands :: [Command]
commands = [Move E, Move W, Move SE, Move SW, Turn CW, Turn CCW]

applyCommand :: Command -> Unit.Unit -> Unit.Unit
applyCommand (Move dir) = Unit.move dir
applyCommand (Turn dir) = Unit.turn dir
