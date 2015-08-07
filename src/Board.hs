module Board where

import Data.Map (Map(..))

import Cell
import Types

data Board = Board { cols :: Number, rows :: Number, fulls :: Map Cell }


