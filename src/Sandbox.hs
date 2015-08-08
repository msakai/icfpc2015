module Sandbox where

import Control.Arrow ((&&&))
import Control.Monad (forM_, forever)
import Data.Maybe (isJust, isNothing, maybe, fromJust)
import System.IO
import Text.PrettyPrint.Boxes (printBox)

import Command
import Game
import Display
import Unit
import Util
import Types

dispProblemStart :: Int -> IO ()
dispProblemStart n = do
  Just inp <- readProblem ("problems/problem_" ++ show n ++ ".json")
  printBox $ dispBoard (initBoard inp) [ spawn (wh inp) (head (snd (head (initSources inp)))) ]
    where
      wh = width &&& height

dispAllProblemsStart :: IO ()
dispAllProblemsStart = forM_ [0..20] $ \n ->
  putStrLn ("Problem : " ++ show n) >> dispProblemStart n

hGetCommand :: Handle -> IO Command
hGetCommand h =  maybe (hGetCommand h) return . keyToCommand =<< hGetChar h

keyToCommand :: Char -> Maybe Command
keyToCommand 'h' = Just (Move W)
keyToCommand 'l' = Just (Move E)
keyToCommand 'j' = Just (Move SW)
keyToCommand 'k' = Just (Move SE)
keyToCommand ' ' = Just (Turn CW)
keyToCommand 'z' = Just (Turn CCW)
keyToCommand _   = Nothing

test :: IO ()
test = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forever $ do
    c <- hGetCommand stdin
    hPutStrLn stdout $ show c
    hFlush stdout
