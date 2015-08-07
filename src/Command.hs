{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
 #-}
module Command where

import Data.Aeson
import GHC.Generics

data MDir = E | W | SE | SW deriving (Show, Eq, Generic)
data CDir = CW | CCW deriving (Show, Eq, Generic)

instance FromJSON MDir
instance ToJSON MDir
instance FromJSON CDir
instance ToJSON CDir

data Command = Move MDir | Turn CDir deriving (Show, Eq, Generic)
type Commands = [Command]

instance FromJSON Command
instance ToJSON Command
instance FromJSON Commands
instance ToJSON Commands
