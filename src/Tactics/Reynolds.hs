module Reynolds
       ( newPlayer
       )where

import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.Set as Set
import qualified System.Random as Rand

import Types
import qualified Board
import Cell
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
evalGS gs = undefined
    where
      b = Game.gsBoard gs
      (h, w) = (Board.rows &&& Board.cols) b
      cs = Board.fulls b

around :: (Number, Number) -> Cell -> [Cell]
around (h, w) (Cell x y) = filter outOfBounds (if odd y then aroundOdd else aroundEven)
    where
      outOfBounds :: Cell -> Bool
      outOfBounds (Cell x y) = 0 <= x && x <= maxW && 0 <= y && y <= maxH
      aroundOdd = [ Cell (x-1) y
                  , Cell x (y-1)
                  , Cell (x+1) (y-1)
                  , Cell (x+1) y
                  , Cell (x+1) (y+1)
                  , Cell x (y+1)
                  ]
      aroundEven = [ Cell (x-1) y
                   , Cell (x-1) (y-1)
                   , Cell x (y-1)
                   , Cell (x+1) y
                   , Cell x (y+1)
                   , Cell (x-1) (y+1)
                   ]
      (maxH, maxW) = (h-1, w-1)
                     
