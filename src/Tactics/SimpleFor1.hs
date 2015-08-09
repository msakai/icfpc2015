module Tactics.SimpleFor1
       ( newPlayer
       ) where

import Types
import Command
import Game (GameState)
import Play

-- From real play for problem_1.json
vanish1Row :: [Command]
vanish1Row = [Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW] ++ vanish1Row

newPlayer :: IO (GameState -> IO Command)
newPlayer = mkReplayPlayer vanish1Row
