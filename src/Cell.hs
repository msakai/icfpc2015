{-# LANGUAGE OverloadedStrings #-}
module Cell where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Types

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq)

instance FromJSON Cell where
  parseJSON (Object v) = Cell <$> v .: "x" <*> v .: "y"
