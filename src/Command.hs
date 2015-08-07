module Command where

data MDir = E | W | SE | SW deriving (Show, Eq)
data CDir = CW | CCW deriving (Show, Eq)

data Command = Move MDir | Turn CDir deriving (Show, Eq)
type Commands = [Command]
