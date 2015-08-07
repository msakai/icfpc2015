{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Cell where

import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Ix
import Data.List

import Data.Aeson
import GHC.Generics

import Types

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq, Generic)

instance FromJSON Cell
instance ToJSON Cell

instance Ord Cell where
  compare = comparing revcoord 

instance Ix Cell where
  range (a,b)     = map revcell (range (revcoord a, revcoord b))
  index (a,b) c   = index (revcoord a, revcoord b) (revcoord c)
  inRange (a,b) c = inRange (revcoord a, revcoord b) (revcoord c)

coord :: Cell -> (Number,Number)
coord = x &&& y

revcoord :: Cell -> (Number,Number)
revcoord = y &&& x

cell :: (Number, Number) -> Cell
cell (x,y) = Cell {x = x, y = y}

revcell :: (Number, Number) -> Cell
revcell (y,x) = Cell {x = x, y = y}

moveCell :: FaceDir -> Cell -> Cell
moveCell dir c@Cell{ x, y } =
  case dir of
    FaceW  -> c{ x = x - 1 }
    FaceE  -> c{ x = x + 1 }
    FaceNW -> Cell{ x = if even y then x - 1 else x, y = y - 1 }
    FaceNE -> Cell{ x = if even y then x else x + 1, y = y - 1 }
    FaceSW -> Cell{ x = if even y then x - 1 else x, y = y + 1 }
    FaceSE -> Cell{ x = if even y then x else x + 1, y = y + 1 }

testCellDir = and
  [ moveCell FaceSE (Cell 1 1) == Cell 2 2
  , moveCell FaceSW (Cell 1 1) == Cell 1 2
  , moveCell FaceNE (Cell 1 1) == Cell 2 0
  , moveCell FaceNW (Cell 1 1) == Cell 1 0

  , moveCell FaceSE (Cell 1 2) == Cell 1 3
  , moveCell FaceSW (Cell 1 2) == Cell 0 3
  , moveCell FaceNE (Cell 1 2) == Cell 1 1
  , moveCell FaceNW (Cell 1 2) == Cell 0 1
  ]
