{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Unit where

import Data.Aeson
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Cell
import Types

data Unit = Unit { members :: !(Set Cell), pivot :: !Cell } deriving (Show, Ord, Eq, Generic)

instance FromJSON Unit
instance ToJSON Unit

size :: Unit -> Pt
size = Set.size . members

spawn :: (Int,Int) -> Unit -> Unit
spawn (w,_) = center w . moveToTop

moveToTop :: Unit -> Unit
moveToTop u =
  if even topmost
     then moveN' FaceNW (topmost `div` 2) $ moveN' FaceNE (topmost `div` 2) $ u
     else moveN' FaceNW (topmost `div` 2 + 1) $ moveN' FaceNE (topmost `div` 2) $ u
  where
    topmost = minimum [ y | Cell{ x, y } <- Set.toList (members u) ]
             
center :: Number -> Unit -> Unit
center w u@Unit{ members } = moveN' FaceE (leftSpace - leftMost) u
  where
    leftMost = minimum [ x | Cell{ x, y } <- Set.toList members ]
    rightMost = maximum [ x | Cell{ x, y } <- Set.toList members ]
    unitWidth = rightMost - leftMost + 1
    leftSpace = (w - unitWidth) `div` 2

moveN' :: FaceDir -> Int -> Unit -> Unit
moveN' dir n Unit{ members, pivot } = Unit{ members = Set.map f members, pivot = f pivot }
  where
    f = moveCellN dir n
        
move :: MDir -> Unit -> Unit
move dir =
  case dir of
    E  -> moveN' FaceE 1
    W  -> moveN' FaceW 1
    SE -> moveN' FaceSE 1
    SW -> moveN' FaceSW 1

turn :: CDir -> Unit -> Unit
turn dir u@Unit{ members, pivot } = u{ members = Set.map f members }
  where
    f = turnCell dir pivot
