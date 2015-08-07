module Util where

import Control.Monad (forM_)
import Data.Aeson hiding (decode')
import Data.ByteString.Lazy.Char8 (pack)
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
  cs <- readFile infile
  writeFile outfile $ show $ decode' cs

decode' :: String -> Maybe Input
decode' = decode . pack

readProblem :: FilePath -> IO (Maybe Input)
readProblem fs = return . decode' =<< readFile fs
