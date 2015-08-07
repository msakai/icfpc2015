{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Unit where

import Data.Aeson
import Data.List
import GHC.Generics
import Cell
import qualified Command as C
import Types

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Eq, Generic)

instance FromJSON Unit
instance ToJSON Unit

move :: C.MDir -> Unit -> Unit
move dir Unit{ members, pivot } = Unit{ members = map f members, pivot = f pivot }
  where
    f =
      case dir of
        C.E  -> moveCell FaceE
        C.W  -> moveCell FaceW
        C.SE -> moveCell FaceSE 
        C.SW -> moveCell FaceSW

turn :: C.CDir -> Unit -> Unit
turn dir u@Unit{ members, pivot } = u{ members = map f members }
  where
    f = turnCell dir pivot
