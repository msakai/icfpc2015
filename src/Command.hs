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
import Data.Maybe (isJust, catMaybes)
import Data.Tuple (swap)
import Data.List (sort,sortBy,group,(\\))
import Data.Ord (comparing)
import GHC.Generics

import Types

data Command = Move MDir | Turn CDir deriving (Show, Read, Eq, Ord, Generic)
type Commands = [Command]

instance FromJSON Command
instance ToJSON Command
instance FromJSON Commands
instance ToJSON Commands

allCommands :: [Command]
allCommands = [Move E, Move W, Move SE, Move SW, Turn CW, Turn CCW]

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

stringToCommands :: String -> Commands
stringToCommands cs = if all isJust ms
                      then catMaybes ms
                      else error ("stringToCommands: " ++ show cs ++ " contains invalid characters")
    where
      ignore ch = notElem ch ['\t', '\n', '\r']
      ms = map (charToCommand . toLower) $ filter ignore cs

commandToChar :: Command -> [Char]
commandToChar cmd = maybe [] id $ M.lookup cmd mapCommandToChars

commandsToString :: Commands -> [String]
commandsToString = map concat . cp . cmdsToStrings

phraseDict :: [(Commands,String)]
phraseDict = map ((stringToCommands &&& Prelude.id) . fst) phraseOfPowers

cmdsToStrings :: Commands -> [[String]]
cmdsToStrings [] = []
cmdsToStrings ccs@(c:cs) = case checkPrefix ccs of
  [] -> cmdToString c : cmdsToStrings cs
  xs -> case unzip xs of
     (ys,zs) -> [head ys] : cmdsToStrings (head zs)

checkPrefix :: Commands -> [(String, Commands)]
checkPrefix cs = [ (s,drop plen cs) | (p,s) <- phraseDict
                                    , let pre = zipWith (==) cs p
                                    , and pre
                                    , let plen = length pre
                                    , length p == plen
                                    ]

cmdToString :: Command -> [String]
cmdToString = map (:[]) . commandToChar

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where
                yss = cp xss

powerPhrases :: [String]
powerPhrases = map fst phraseOfPowers

phraseOfPowers :: [(String,Int)]
phraseOfPowers = sortBy (flip (comparing snd)) $ map ((,) <*> length) $
    ["Ei!"
    ,"Ia! Ia!"
    ,"R'lyeh"
    ,"Yuggoth"
    ,"Tsathogua"
    ,"YogSothoth"
    ,"Necronomicon"
    ,"Vigintillion"
    ,"Cthulhu fhtagn!"
    ,"The Laundry"
    ,"Planet 10"
    ,"Yoyodyne"
    ,"Monkeyboy"
    ,"John Bigboote"
    ,"Blue Hades"
    ,"Case Nightmare Green"
    ,"In his house at R'lyeh dead Cthulhu waits dreaming."
    ,"Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn."
    ]
