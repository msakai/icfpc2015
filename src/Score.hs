module Score where

import Control.Applicative ((<*>))
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

power_score :: Commands -> Set.Set Commands -> (Pt, Set.Set Commands)
power_score cmds poccs = case checkRevCmdPrefix cmds of
  []           -> (0,poccs)
  (pcmds,pt):_ -> if Set.member pcmds poccs then (pt,poccs)
                  else (pt + power_bonus,Set.insert pcmds poccs)
                  where power_bonus = 300

revcmds :: [(Commands,Pt)]
revcmds = map (f . reverse) $ fst $ unzip phraseDict
  where
    f = (,) <*> ((2*) . length)

checkRevCmdPrefix :: Commands -> [(Commands,Pt)]
checkRevCmdPrefix ts = [ e | e@(cs,pt) <- revcmds
                           , let pre = zipWith (==) ts cs
                           , and pre
                           , let plen = length pre
                           , length cs == plen
                           ]
