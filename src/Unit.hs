{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , NamedFieldPuns
 #-}
module Unit where

import Data.Aeson
import Data.List
import GHC.Generics
import Cell
import qualified Command as C
import Types

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Eq, Generic)

instance FromJSON Unit
instance ToJSON Unit

move :: C.MDir -> Unit -> Unit
move dir Unit{ members, pivot } = Unit{ members = map f members, pivot = f pivot }
  where
    f =
      case dir of
        C.E  -> moveCell FaceE
        C.W  -> moveCell FaceW
        C.SE -> moveCell FaceSE 
        C.SW -> moveCell FaceSW

turn :: C.CDir -> Unit -> Unit
turn dir u@Unit{ members, pivot } = u{ members = map f members }
  where
    f = turnCell dir pivot

turnFaceDir :: C.CDir -> FaceDir -> FaceDir
turnFaceDir C.CW  = f
  where
    f FaceNW = FaceNE
    f FaceNE = FaceE
    f FaceE  = FaceSE
    f FaceSE = FaceSW
    f FaceSW = FaceW
    f FaceW  = FaceNW
turnFaceDir C.CCW = f
  where
    f FaceNE = FaceNW
    f FaceE  = FaceNE
    f FaceSE = FaceE
    f FaceSW = FaceSE
    f FaceW  = FaceSW
    f FaceNW = FaceW

turnCell :: C.CDir -> Cell -> Cell -> Cell
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

test_turnCell = and
  [ turnCell C.CW (Cell 2 2) (Cell 2 2) == Cell 2 2
  , turnCell C.CCW (Cell 2 2) (Cell 2 2) == Cell 2 2

  , turnCell C.CW (Cell 2 3) (Cell 2 3) == Cell 2 3
  , turnCell C.CCW (Cell 2 3) (Cell 2 3) == Cell 2 3

  , turnCell C.CW (Cell 2 2) (Cell 3 5) == Cell 0 5
  , turnCell C.CCW (Cell 2 2) (Cell 0 5) == Cell 3 5

  , turnCell C.CW (Cell 2 2) (Cell 4 4) == Cell 1 5
  , turnCell C.CCW (Cell 2 2) (Cell 1 5) == Cell 4 4

  , turnCell C.CW (Cell 2 3) (Cell 3 5) == Cell 1 5
  , turnCell C.CCW (Cell 2 3) (Cell 1 5) == Cell 3 5

  , turnCell C.CW (Cell 2 3) (Cell 4 4) == Cell 2 5
  , turnCell C.CCW (Cell 2 3) (Cell 2 5) == Cell 4 4
  ]
