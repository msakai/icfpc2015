module Tactics.SimpleFor1
       ( player
       ) where

import Types
import Command
import Game (GameState)

-- From real play for problem_1.json
banish1Row :: [Command]
banish1Row = [Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move E,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move E,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SE,Move SW,Move SE,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SW,Move SE,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW,Move SW,Move SW,Move SE,Move SE,Move SW,Move SW,Move SE,Move SE,Move SE,Move SW,Move SW,Move W,Move W,Move W,Move W,Move W,Move SW,Move SW,Move SW,Move SW] ++ banish1Row

player :: GameState -> IO Commands
player _ = return banish1Row
