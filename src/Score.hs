module Score where

import Control.Applicative ((<*>))
import Data.List (group,sort,unfoldr,isPrefixOf)
import qualified Data.Set as Set

import Types
import Command

move_score :: Pt -> Pt -> Pt -> Pt
move_score size ls ls_old = points + line_bonus
    where
      points = size + 50 * (1 + ls) * ls
      line_bonus = if ls_old > 1
                   then ((ls_old - 1) * points `div` 10)
                   else 0

power_score :: Commands -> Pt
power_score cmds = case countPhrases (head $ commandsToString $ reverse cmds) of
  [] -> 0
  xs -> ((+) . sum . map snd <*> (300 *) . length . group . sort) xs

countPhrases :: String -> [(String,Int)]
countPhrases = unfoldr phi
  where
    phi [] = Nothing
    phi xs@(_:rs) = case checkPrefixString phraseOfPowers xs of
      []        -> phi rs
      (p,len):_ -> Just ((p,len*2),drop len xs)

checkPrefixString :: [(String,Int)] -> String -> [(String,Int)]
checkPrefixString pps cmdstr
  = [ pp | pp@(p,_) <- pps, isPrefixOf p cmdstr ]

revcmds :: [(Commands,Pt)]
revcmds = map (f . reverse) $ fst $ unzip phraseDict
  where
    f = (,) <*> ((2*) . length)

checkRevCmdPrefix :: Commands -> [(Pt,Commands)]
checkRevCmdPrefix ts
  = [(pt,drop clen ts) | e@(cs,pt) <- revcmds
                       , let pre = zipWith (==) ts cs
                       , and pre
                       , let plen = length pre
                       , let clen = length cs
                       , clen == plen
                       ]
