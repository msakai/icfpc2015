module Board where

import Data.Ix (inRange,range)
import Data.List (foldl')
import Data.Set (Set(..))
import qualified Data.Set as Set
  (notMember,insert,size,filter,split,union,mapMonotonic,fromList,elems)

import Cell
import Unit
import Types

data Board = Board { cols :: Number, rows :: Number, fulls :: Set Cell } deriving (Show,Eq)


valid :: Board -> Unit -> Maybe Unit
valid b u 
 = if and (map (flip Set.notMember fs) cs) && all (inRange (cell (0,0), cell (cols b -1, rows b -1))) cs
   then Just u else Nothing
  where
    fs = fulls b
    cs = members u

lockUnit :: Board -> Unit -> Board
lockUnit b u = b { fulls = foldr Set.insert (fulls b) (members u) }

findFullRows :: Board -> [Number]
findFullRows b = filter fullRow [h-1,h-2 .. 0]
  where
    w = cols b
    h = rows b
    fs = fulls b
    fullRow r = Set.size (Set.filter ((r==) . y) fs) == w
    
clearFullRows :: Board -> Board
clearFullRows b = foldl' clearRow b (findFullRows b)

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
