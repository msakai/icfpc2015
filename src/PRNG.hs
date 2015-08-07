module PRNG
  ( Seed
  , generate
  ) where

import Data.Bits
import Data.Word

type Seed = Word32

modulus = 2^32
multiplier = 1103515245
increment = 12345

nextSeed :: Seed -> Seed 
nextSeed seed = multiplier * seed + increment

getValue :: Seed -> Int
getValue seed = fromIntegral $ (seed `shiftR` 16) .&. (2^(15::Int)-1)

generate :: Seed -> [Int]
generate = map getValue . iterate nextSeed

test = take 10 (generate 17) == [0,24107,16552,12125,9427,13152,21440,3383,6873,16117]
