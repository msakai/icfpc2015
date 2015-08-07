module Game where

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
             deriving (Show, Eq)


data OutputItem = OutputItem { problemId :: Number
                             , seed :: Number
                             , tag :: String
                             , solution :: Commands
                             }
                deriving (Show, Eq)

