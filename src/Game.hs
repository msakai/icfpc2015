{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
 #-}
module Game where

import Data.Aeson
import qualified Data.Set as Set
import Data.Ix (inRange)
import GHC.Generics

import Board
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

type Output = [OutputItem]

instance FromJSON OutputItem
instance ToJSON OutputItem
instance FromJSON Output
instance ToJSON Output

initBoard :: Input -> Board
initBoard i = Board { cols = width i, rows = height i, fulls = Set.fromList (filled i) }

initSource :: Input -> [Unit]
initSource =  undefined
