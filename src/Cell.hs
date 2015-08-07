{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
 #-}
module Cell where

import Data.Aeson
import GHC.Generics

import Types

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq, Generic)

instance FromJSON Cell
instance ToJSON Cell
