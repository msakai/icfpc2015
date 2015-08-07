module Board where

import Data.Ix (inRange)
import Data.Set (Set(..),notMember)

import Cell
import Unit
import Types

data Board = Board { cols :: Number, rows :: Number, fulls :: Set Cell }


valid :: Board -> Unit -> Maybe Unit
valid b u 
 = if and (map (flip notMember fs) cs) && all (inRange (cell (0,0), cell (cols b -1, rows b -1))) cs
   then Just u else Nothing
  where
    fs = fulls b
    cs = members u
