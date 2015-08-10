module Reynolds
       ( newPlayer
       )where

import Data.List (sort)
import qualified System.Random as Rand

import Types
import Command
import qualified Game
import qualified Unit
import Play

newPlayer :: IO Player
newPlayer = do
  rs <- sequence $ take 10 $ repeat Rand.newStdGen
  return $ player rs

player :: Rand.RandomGen r => [r] -> Player
player rs = do
  gs <- getGameState
  let futures = map (uncurry runTillLock) $ zip rs (repeat gs)
      cs = best futures
  return undefined

runTillLock :: Rand.RandomGen r => r -> Game.GameState -> Game.GameState
runTillLock r gs = undefined

best :: [Game.GameState] -> Game.GameState
best = head . sort

instance Eq Game.GameState where
  x == y = evalGS x == evalGS y

instance Ord Game.GameState where
  compare x y = compare (evalGS x) (evalGS y)

evalGS :: Game.GameState -> Int
evalGS = undefined
