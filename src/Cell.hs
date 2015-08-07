{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Cell where

import Control.Arrow ((&&&))
import Data.Aeson
import GHC.Generics

import Types

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq, Ord, Generic)

instance FromJSON Cell
instance ToJSON Cell

coord :: Cell -> (Number,Number)
coord = x &&& y

eastCell :: Cell -> Cell
eastCell c@Cell{ x } = c{ x = x + 1 }

westCell :: Cell -> Cell
westCell c@Cell{ x } = c{ x = x - 1 }

southEastCell :: Cell -> Cell
southEastCell c@Cell{ x, y } = Cell{ x = if even y then x else x + 1, y = y + 1 }

southWestCell :: Cell -> Cell
southWestCell c@Cell{ x, y } = Cell{ x = if even y then x - 1 else x, y = y + 1 }

northEastCell :: Cell -> Cell
northEastCell c@Cell{ x, y } = Cell{ x = if even y then x else x + 1, y = y - 1 }

northWestCell :: Cell -> Cell
northWestCell c@Cell{ x, y } = Cell{ x = if even y then x - 1 else x, y = y - 1 }

testCellDir = and
  [ southEastCell (Cell 1 1) == Cell 2 2
  , southWestCell (Cell 1 1) == Cell 1 2
  , northEastCell (Cell 1 1) == Cell 2 0
  , northWestCell (Cell 1 1) == Cell 1 0

  , southEastCell (Cell 1 2) == Cell 1 3
  , southWestCell (Cell 1 2) == Cell 0 3
  , northEastCell (Cell 1 2) == Cell 1 1
  , northWestCell (Cell 1 2) == Cell 0 1
  ]
