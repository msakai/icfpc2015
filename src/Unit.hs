module Unit where

import Cell

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Eq)
