module Board where

import Data.Ix (inRange)
import Data.Set (Set(..))
import qualified Data.Set as Set (notMember,insert)

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
