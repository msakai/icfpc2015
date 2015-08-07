module Honycomb where

type Number = Int

data Input = Input { id :: Int
                   , units :: [Unit]
                   , width :: Int
                   , height :: Int
                   , filled :: [Cell]
                   , sourceLength :: Number
                   , sourceSeeds :: [Number]
                   }
             deriving (Show, Eq)

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq)

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Eq)

data MDir = E | W | SE | SW deriving (Show, Eq)
data CDir = CW | CCW deriving (Show, Eq)

data Command = Move MDir | Turn CDir deriving (Show, Eq)

data OutputItem = OutputItem { problemId :: Number
                             , seed :: Number
                             , tag :: String
                             , solution :: Commands
                             }
                deriving (Show, Eq)

type Commands = [Command]
