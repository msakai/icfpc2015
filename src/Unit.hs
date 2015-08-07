{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Unit where

import Data.Aeson
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
        C.E  -> eastCell
        C.W  -> westCell
        C.SE -> southEastCell
        C.SW -> southWestCell
