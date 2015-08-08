{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Cell where

import Control.Arrow ((&&&))
import Data.Ord (comparing)
import Data.Ix
import Data.List

import Data.Aeson
import GHC.Generics

import Command (CDir (..))
import Types

data Cell = Cell { x :: Number , y :: Number } deriving (Show, Eq, Generic)

instance FromJSON Cell
instance ToJSON Cell

instance Ord Cell where
  compare = comparing revcoord 

instance Ix Cell where
  range (a,b)     = map revcell (range (revcoord a, revcoord b))
  index (a,b) c   = index (revcoord a, revcoord b) (revcoord c)
  inRange (a,b) c = inRange (revcoord a, revcoord b) (revcoord c)

coord :: Cell -> (Number,Number)
coord = x &&& y

revcoord :: Cell -> (Number,Number)
revcoord = y &&& x

cell :: (Number, Number) -> Cell
cell (x,y) = Cell {x = x, y = y}

revcell :: (Number, Number) -> Cell
revcell (y,x) = Cell {x = x, y = y}

moveCell :: FaceDir -> Cell -> Cell
moveCell dir c@Cell{ x, y } =
  case dir of
    FaceW  -> c{ x = x - 1 }
    FaceE  -> c{ x = x + 1 }
    FaceNW -> Cell{ x = if even y then x - 1 else x, y = y - 1 }
    FaceNE -> Cell{ x = if even y then x else x + 1, y = y - 1 }
    FaceSW -> Cell{ x = if even y then x - 1 else x, y = y + 1 }
    FaceSE -> Cell{ x = if even y then x else x + 1, y = y + 1 }

turnFaceDir :: CDir -> FaceDir -> FaceDir
turnFaceDir CW  = f
  where
    f FaceNW = FaceNE
    f FaceNE = FaceE
    f FaceE  = FaceSE
    f FaceSE = FaceSW
    f FaceSW = FaceW
    f FaceW  = FaceNW
turnFaceDir CCW = f
  where
    f FaceNE = FaceNW
    f FaceE  = FaceNE
    f FaceSE = FaceE
    f FaceSW = FaceSE
    f FaceW  = FaceSW
    f FaceNW = FaceW

turnCell :: CDir -> Cell -> Cell -> Cell
turnCell dir pivot@Cell{ x = px, y = py } = fromPath . map (turnFaceDir dir) . toPath
  where
    toPath :: Cell -> [FaceDir]
    toPath Cell{ x, y } = path1 ++ path2 ++ path3
      where
        path1 = if x <= px then replicate (px - x) FaceW else replicate (x - px) FaceE
        path2 = if y <= py
                then concat $ replicate ((py - y) `div` 2) [FaceNE, FaceNW]
                else concat $ replicate ((y - py) `div` 2) [FaceSE, FaceSW]
        path3
          | even py == even y = []
          | even py = if y <= py then [FaceNE] else [FaceSE] -- even py && odd y
          | otherwise = if y <= py then [FaceNW] else [FaceSW] -- odd py && even y

    fromPath :: [FaceDir] -> Cell
    fromPath = foldl' (flip moveCell) pivot
