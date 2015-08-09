module Board where

import Control.Arrow ((&&&))
import Data.Ix (inRange,range)
import Data.List (foldl')
import Data.Set (Set(..))
import qualified Data.Set as Set

import Cell
import Unit
import Types

import Debug.Trace

data Board = Board { cols :: Number, rows :: Number, fulls :: Set Cell } deriving (Show,Eq)


valid :: Board -> Unit -> Bool
valid b u 
 = notBatting && withinRange
   where
     notBatting  = Set.null $ fs `Set.intersection` cs
     withinRange = all (inRange (cell (0,0), cell (cols b -1, rows b -1))) (Set.toList cs)
     fs = fulls b
     cs = members u

lockUnit :: Board -> Unit -> Board
lockUnit b u = b { fulls = fulls b `Set.union` members u }

findFullRows :: Board -> [Number]
findFullRows b = filter fullRow [0 .. h-1]
  where
    w = cols b
    h = rows b
    fs = fulls b
    fullRow r = Set.size (Set.filter ((r==) . y) fs) == w
    
clearFullRows :: Board -> (Int, Board)
clearFullRows b = (length &&& foldl' clearRow b) cleared
    where
      cleared = findFullRows b

clearRow :: Board -> Number -> Board
clearRow b r = case Set.split (Cell { x = 0, y = r }) fs of
  (ps,qs) -> b { fulls = Set.mapMonotonic (\ c -> c { y = y c + 1 }) ps `Set.union` qs }
  where 
    fs = Set.filter ((r /=) . y) (fulls b)

--

sampleBoard :: Board
sampleBoard = Board { cols = 5, rows = 10
                    , fulls = Set.fromList (range (cell (1,0), cell (3,4)))
                              `Set.union`
                              Set.fromList (range (cell (0,1),cell (4,3))) }
