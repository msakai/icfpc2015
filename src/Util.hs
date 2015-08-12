module Util
       ( readProblem
       , readOutput
       ) where

import Control.Monad (forM_, liftM)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Directory (createDirectoryIfMissing)

import Game

convertProblemJsonToDat :: IO ()
convertProblemJsonToDat = do
  createDirectoryIfMissing True "problems/decoded"
  convert [0..20]

convert :: [Int] -> IO ()
convert ns = forM_ ns $ \n -> do
  let (infile, outfile) = ("problems/problem_" ++ show n ++ ".json",
                           "problems/decoded/problem_" ++ show n ++ ".dat"
                          )
  cs <- LBS.readFile infile
  writeFile outfile $ show $ (decode cs :: Maybe Input)

readProblem :: FilePath -> IO (Maybe Input)
readProblem fs = liftM decode $ LBS.readFile fs

readOutput :: FilePath -> IO (Maybe Output)
readOutput = liftM decode . LBS.readFile

