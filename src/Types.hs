{-# LANGUAGE
    DeriveGeneric
 #-}
module Types where

import Data.Aeson
import GHC.Generics    

type Number = Int
type Pt = Int

data MDir = E | W | SE | SW deriving (Show, Eq, Ord, Generic)
data CDir = CW | CCW deriving (Show, Eq, Ord, Generic)

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

oppositeFaceDir :: FaceDir -> FaceDir
oppositeFaceDir FaceE = FaceW
oppositeFaceDir FaceW = FaceE
oppositeFaceDir FaceNW = FaceSE
oppositeFaceDir FaceNE = FaceSW
oppositeFaceDir FaceSE = FaceNW
oppositeFaceDir FaceSW = FaceNE
