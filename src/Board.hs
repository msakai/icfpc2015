module Board where

import Data.Set (Set(..))

import Cell
import Types

data Board = Board { cols :: Number, rows :: Number, fulls :: Set Cell }
