{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
 #-}
module Game where

import Data.Aeson
import GHC.Generics

import Types
import Cell
import Unit
import Command

data Input = Input { id :: Number
                   , units :: [Unit]
                   , width :: Number
                   , height :: Number
                   , filled :: [Cell]
                   , sourceLength :: Number
                   , sourceSeeds :: [Number]
                   }
             deriving (Show, Eq, Generic)

instance FromJSON Input
instance ToJSON Input

data OutputItem = OutputItem { problemId :: Number
                             , seed :: Number
                             , tag :: String
                             , solution :: Commands
                             }
                deriving (Show, Eq, Generic)

instance FromJSON OutputItem
instance ToJSON OutputItem
