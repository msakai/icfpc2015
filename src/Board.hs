module Board where

import Data.Ix (inRange)
import Data.List (foldl')
import Data.Set (Set(..))
import qualified Data.Set as Set
  (notMember,insert,size,filter,split,union,mapMonotonic)

import Cell
import Unit
import Types

data Board = Board { cols :: Number, rows :: Number, fulls :: Set Cell }


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
  (ps,qs) -> b { fulls = ps `Set.union` Set.mapMonotonic (\ c -> c { y = y c - 1 }) qs }
  where 
    fs = Set.filter ((r /=) . y) (fulls b)
