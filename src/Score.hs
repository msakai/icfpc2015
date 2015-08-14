module Score where

import Control.Arrow ((&&&))
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.List (group,sort,sortBy,unfoldr,isPrefixOf)
import Data.Ord (comparing)
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

power_score :: [String] -> Commands -> Pt
power_score phs cmds = fst $ power_score' phs cmds

power_score' :: [String] -> Commands -> (Pt, String)
power_score' phs cmds = case countPhrases phs s of
  [] -> (0, s)
  xs -> ((uncurry (+) . (sum . map snd &&& (300 *) . length . group . sort)) xs, s)
  where
    s = head $ commandsToString $ reverse cmds
        
countPhrases :: [String] -> String -> [(String,Int)]
countPhrases phs = unfoldr phi
  where
    phi [] = Nothing
    phi xs@(_:rs) = case checkPrefixString phs' xs of
      []        -> phi rs
      (p,len):_ -> Just ((p,len*2),drop len xs)
    phs' = sortBy (flip (comparing snd)) $ map (id &&& length) $ phs

checkPrefixString :: [(String,Int)] -> String -> [(String,Int)]
checkPrefixString pps cmdstr
  = [ pp | pp@(p,_) <- pps, isPrefixOf p cmdstr ]

revcmds :: [(Commands,Pt)]
revcmds = map (f . reverse) $ fst $ unzip phraseDict
  where
    f = id &&& (2*) . length

checkRevCmdPrefix :: Commands -> [(Pt,Commands)]
checkRevCmdPrefix ts
  = [(pt,drop clen ts) | e@(cs,pt) <- revcmds
                       , let pre = zipWith (==) ts cs
                       , and pre
                       , let plen = length pre
                       , let clen = length cs
                       , clen == plen
                       ]

computePowerScoreFromString :: [String] -> String -> Pt
computePowerScoreFromString ps s = sum $ do
  p <- ps
  let p' = BS.pack p
      resp = length $ BS.findSubstrings p' s'
  guard $ resp > 0
  return $ 2 * BS.length p' * resp + 300
  where
    s' = BS.pack s
