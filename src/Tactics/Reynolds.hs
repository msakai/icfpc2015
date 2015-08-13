module Tactics.Reynolds
       ( newPlayer
       , evalGS
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
import Tactics.Util (randomWalkTillLockedWithPPs)

newPlayer :: Int -> IO Player
newPlayer n = do
  rs <- sequence $ take n $ repeat Rand.newStdGen
  return $ player [cmds | pp <- phrases, let cmds = stringToCommands pp] rs

player :: Rand.RandomGen r => [Commands] -> [r] -> Player
player pps rs = do
  gs <- getGameState
  let futures = [randomWalkTillLockedWithPPs pps r gs | r <- rs]
      (cmds,gs',r') = maximumBy (comparing (\(cmds,gs',r') -> evalGS gs')) futures
  mapM_ command cmds
  player pps [r' | (cmds,gs',r') <- futures]

evalGS :: Game.GameState -> Float
evalGS gs = (reynolds * position * sitdown * score) ^(1 + Game.gsLs gs)
    where
      b = Game.gsBoard gs
      hw@(h, w) = (Board.rows &&& Board.cols) b
      cs = Board.fulls b
      aroundcs = around hw

      reynolds = fromIntegral volume / fromIntegral surface
      volume = Set.size cs
      surface = sum [fact $ length $ filter spacep $ aroundcs c | c <- Set.toList cs]
      spacep :: Cell -> Bool
      spacep c = Board.isEmptyCell b c
      fact 0 = 1
      fact n = n * fact (n-1)

      -- low level is value and around spaces is value
      position = sum [(pos c)^2 + aroundVal c | c <- Set.toList cs]
      pos (Cell x y) = fromIntegral x + fromIntegral y
      aroundVal c = besideOf (xs !! 0) + aboveOf (xs !! 1) + aboveOf(xs !! 2) +
                    besideOf (xs !! 3) + belowOf (xs !! 4) + belowOf (xs !! 5)
          where
            xs = virtualAroundCells c
            besideOf c = if outOfBounds hw c
                         then 5
                         else if spacep c then -1 else 1
            aboveOf c = if outOfBounds hw c
                        then -5
                        else if spacep c then -3 else 1
            belowOf c = if outOfBounds hw c
                        then 5
                        else if spacep c then -5 else 3

      -- sit down is value
      sitdown = fromIntegral $ (fromInteger highwatermark)^10
      highwatermark = if Set.null cs then (toInteger h-1) else minimum $ map (toInteger.height) $ Set.toList cs
      height (Cell x y) = y

      -- ls is good
      ls = fromIntegral $ 100 * Game.gsLs gs

      -- score is good
      score = fromIntegral $ Game.gsScore gs

around :: (Number, Number) -> Cell -> [Cell]
around hw@(h, w) c@(Cell x y) = filter (outOfBounds hw) $ virtualAroundCells c

outOfBounds :: (Number, Number) -> Cell -> Bool
outOfBounds (h, w) (Cell x y) = 0 <= x && x <= maxW && 0 <= y && y <= maxH
    where
      (maxH, maxW) = (h-1, w-1)

virtualAroundCells :: Cell -> [Cell]
virtualAroundCells c@(Cell x y) = if odd y then aroundOdd c else aroundEven c

aroundOdd :: Cell -> [Cell]
aroundOdd (Cell x y)  = [ Cell (x-1) y
                        , Cell x (y-1)
                        , Cell (x+1) (y-1)
                        , Cell (x+1) y
                        , Cell (x+1) (y+1)
                        , Cell x (y+1)
                        ]
aroundEven :: Cell -> [Cell]
aroundEven (Cell x y) = [ Cell (x-1) y
                        , Cell (x-1) (y-1)
                        , Cell x (y-1)
                        , Cell (x+1) y
                        , Cell x (y+1)
                        , Cell (x-1) (y+1)
                        ]
