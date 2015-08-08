module Types where

type Number = Int
type Pt = Int

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
