module Score where

import Types

move_score :: Pt -> Pt -> Pt -> Pt
move_score size ls ls_old = points + line_bonus
    where
      points = size + 50 * (1 + ls) * ls
      line_bonus = if ls_old > 1
                   then ((ls_old - 1) * points `div` 10)
                   else 0
