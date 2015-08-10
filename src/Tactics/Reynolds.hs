module Tactics.Reynolds
       ( newPlayer
       )where

import Control.Arrow ((&&&))
import qualified Data.Set as Set
import Data.List
import Data.Ord (comparing)
import qualified System.Random as Rand

import Types
import qualified Board
import Cell
import Command
import qualified Game
import qualified Unit
import Play
import Tactics.Util (randomWalkTillLocked)

newPlayer :: Int -> IO Player
newPlayer n = do
  rs <- sequence $ take n $ repeat Rand.newStdGen
  return $ player rs

player :: Rand.RandomGen r => [r] -> Player
player rs = do
  gs <- getGameState
  let futures = [randomWalkTillLocked r gs | r <- rs]
      (cmds,gs',r') = maximumBy (comparing (\(cmds,gs',r') -> evalGS gs')) futures
  mapM_ command cmds
  player [r' | (cmds,gs',r') <- futures]

evalGS :: Game.GameState -> Float
evalGS gs = (reynolds * position * sitdown * score) ^(1 + Game.gsLs gs)
    where
      b = Game.gsBoard gs
      hw@(h, w) = (Board.rows &&& Board.cols) b
      cs = Board.fulls b

      reynolds = fromIntegral volume / fromIntegral surface
      volume = length cs
      surface = foldr (\c x -> x + (fact $ length $ filter spacep $ around hw c)) 0 cs
      spacep :: Cell -> Bool
      spacep c = c `notElem` cs
      fact 0 = 1
      fact n = n * fact (n-1)

      -- low level is value
      position = foldr (\c x -> (pos c)^2 + x) 0 cs
      pos (Cell x y) = fromIntegral x + fromIntegral y

      -- sit down is value
      sitdown = fromIntegral $ (fromInteger highwatermark)^10
      highwatermark = minimum $ map (toInteger.height) $ Set.toList cs
      height (Cell x y) = y

      -- ls is good
      ls = fromIntegral $ 100 * Game.gsLs gs

      -- score is good
      score = fromIntegral $ Game.gsScore gs

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
