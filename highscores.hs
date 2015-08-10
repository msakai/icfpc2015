{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import Data.List (groupBy, sortBy, isSuffixOf)

type Pt = Int
type Pid = String
type Sid = String

data Result = Result { pt :: Pt
                     , pid :: Pid
                     , sid :: Sid
                     , filename :: FilePath
                     } deriving (Show)

key :: Result -> String
key r = pid r ++ sid r

instance Eq Result where
  x == y = pt x == pt y
instance Ord Result where
  x <= y = pt x <= pt y

main = do
  (dir:_) <- getArgs
  files <- getDirectoryContents dir
  let rs = map toResult $ filter (isSuffixOf ".json") files
  let fs = map (filename.maximum) $ groupBy hash $ sortBy hash' rs
  forM_ fs $ \f ->
    putStrLn (dir ++ "/" ++ f)
  where
    hash x y = key x == key y
    hash' x y = compare (key x) (key y)

toResult :: FilePath -> Result
toResult fn = Result pt' pid sid fn

  where
    pt' :: Pt
    pt' = read $ take (length pt - 2) pt
    (pt:pid:sid:_) = split fn

split :: String -> [String]
split s = case dropWhile (=='-') s of
  "" -> []
  s' -> w : split s''
      where (w, s'') = break (=='-') s'
