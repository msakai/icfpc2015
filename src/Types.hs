{-# LANGUAGE
    DeriveGeneric
 #-}
module Types where

import Data.Aeson
import GHC.Generics    

type Number = Int
type Pt = Int
type ProblemId = Int
type WaituS = Integer
type Tag = String

data MDir = E | W | SE | SW deriving (Show, Eq, Ord, Enum, Bounded, Generic)
data CDir = CW | CCW deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON MDir
instance ToJSON MDir
instance FromJSON CDir
instance ToJSON CDir

data FaceDir
  = FaceNW
  | FaceNE
  | FaceE
  | FaceSE
  | FaceSW
  | FaceW
  deriving (Show, Eq, Ord, Enum, Bounded)

oppositeFaceDir :: FaceDir -> FaceDir
oppositeFaceDir FaceE = FaceW
oppositeFaceDir FaceW = FaceE
oppositeFaceDir FaceNW = FaceSE
oppositeFaceDir FaceNE = FaceSW
oppositeFaceDir FaceSE = FaceNW
oppositeFaceDir FaceSW = FaceNE

turnFaceDir :: CDir -> FaceDir -> FaceDir
turnFaceDir CW = f
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
