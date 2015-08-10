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
import Data.List (sortBy,group,(\\))
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
commandsToString = commandsToString'

-- commandsToString = combi . map commandToChar
--     where
--       combi :: [String] -> [String]
--       combi [] = [[]]
--       combi (x:xs) = [ c:s | c <- x, s <- combi xs ]

commandsToString' :: Commands -> [String]
commandsToString' = map concat . cp . cmdsToStrings

phrases :: [String]
phrases =  sortBy (flip (comparing length)) 
           ["Ei!"
           ,"Ia! Ia!"
           ,"R'lyeh"
           ,"Yuggoth"
           ]

{-
phrases :: [String]
phrases = (\\ ["Ia!","fhtagn"])
        $ map head
        $ group 
        $ sortBy (flip (comparing length))
        $ (["Ei!"
           ,"Ia! Ia!"
           ,"R'lyeh"
           ,"Yuggoth"
           ,"Cthulhu fhtagn!"
           ] ++ extra51 : words extra51 ++ extra23 : words extra23
          )
-}
extra51,extra23 :: String
extra51 = "Ph'nglui mglw'nafh Cthulhu R'lyeh wgah'nagl fhtagn"
extra23 = "Ia! Ia! Cthulhu fhtagn!"

phraseDict :: [(Commands,String)]
phraseDict = map (stringToCommands &&& Prelude.id) phrases


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
