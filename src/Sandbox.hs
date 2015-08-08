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

display :: GameState -> IO ()
display s = do
  putStrLn $ "Problem " ++ show (gsProblemId s)
  putStrLn $ "Seed " ++ show (gsSeed s)
  putStrLn $ "Tag " ++ show (gsTag s)
  putStrLn $ "-------------"
  printBox $ dispBoard (gsBoard s) [ gsCurUnit s ]
  putStrLn $ "-------------"
  putStrLn $ "CurUnit " ++ show (gsCurUnit s)
  putStrLn $ "Locked " ++ show (gsLocked s)
  putStrLn $ "Over " ++ show (gsOver s)

test :: Int -> String -> IO ()
test n tg = do
  Just inp <- readProblem ("problems/problem_" ++ show n ++ ".json")
  let gss = initGameStates tg inp
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  display (head gss)
  step (head gss)
  where
    step s = do
      c <- hGetCommand stdin
      let s' = gameStep c s
      display s'
      step s'
