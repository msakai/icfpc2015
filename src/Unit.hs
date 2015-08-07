{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
 #-}
module Unit where

import Data.Aeson
import GHC.Generics
import Cell

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Eq, Generic)

instance FromJSON Unit
instance ToJSON Unit
