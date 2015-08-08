module Sandbox where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Text.PrettyPrint.Boxes (printBox)

import Game
import Display
import Unit
import Util

dispProblemStart :: Int -> IO ()
dispProblemStart n = do
  Just inp <- readProblem ("problems/problem_" ++ show n ++ ".json")
  printBox $ dispBoard (initBoard inp) [ spawn (wh inp) (head (head (initSources inp))) ]
    where
      wh = width &&& height

dispAllProblemsStart :: IO ()
dispAllProblemsStart = forM_ [0..20] $ \n ->
  putStrLn ("Problem : " ++ show n) >> dispProblemStart n
