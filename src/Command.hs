{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , TypeSynonymInstances
  , FlexibleInstances
 #-}
module Command where

import Control.Arrow ((&&&), (***))
import Data.Aeson
import Data.Char (toLower, toUpper)
import qualified Data.Map as M hiding (map)
import Data.Maybe (isJust)
import Data.Tuple (swap)
import GHC.Generics

import Types

data Command = Move MDir | Turn CDir deriving (Show, Eq, Ord, Generic)
type Commands = [Command]

instance FromJSON Command
instance ToJSON Command
instance FromJSON Commands
instance ToJSON Commands

dict :: [(Command, String)]
dict =  [ (Move W,   "p'!.03")
        , (Move E,   "bcefy2")
        , (Move SW,  "aghij4")
        , (Move SE,  "lmno 5")
        , (Turn CW,  "dqrvz1")
        , (Turn CCW, "kstuwx")
        ]

mapCommandToChars :: M.Map Command [Char]
mapCommandToChars = M.fromList dict

mapCharToCommand :: M.Map Char Command
mapCharToCommand = M.fromList $ concatMap (uncurry zip . (id *** repeat) . swap) dict

charToCommand :: Char -> Maybe Command
charToCommand ch = M.lookup ch mapCharToCommand

stringToCommands :: String -> [Command]
stringToCommands cs = if all isJust ms
                      then catMaybe ms
                      else []
    where
      ignore ch = notElem ch ['\t', '\n', '\r']
      ms = map (charToCommand . toLower) $ filter ignore cs
      catMaybe xs = [x | Just x <- xs]

commandToChar :: Command -> [Char]
commandToChar cmd = maybe [] id $ M.lookup cmd mapCommandToChars

commandsToString :: Commands -> [String]
commandsToString = combi . map commandToChar
    where
      combi :: [String] -> [String]
      combi [] = [[]]
      combi (x:xs) = [ c:s | c <- x, s <- combi xs ]
