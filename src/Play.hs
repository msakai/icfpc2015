module Play where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Maybe (isJust, isNothing, maybe, fromJust)
import System.IO
import Text.PrettyPrint.Boxes (printBox)

import Command
import Game
import Display
import Unit
import Util
import Types

data Meta = None | Dump | Quit deriving (Show, Eq)

initGameIO :: Int -> String-> IO [GameState]
initGameIO n tg = do
  { input <- readProblem ("problems/problem_"++show n++".json")
  ; case input of { Nothing -> return []; Just inp -> return (initGameStates tg inp) }
  }

testPlay :: Int -> IO ()
testPlay n = do
  gms <- initGameIO n "test-tag"
  runStateT play (head gms)
  return ()

play :: Game ()
play = do 
  { liftIO (hSetBuffering stdin NoBuffering)
  ; liftIO (hSetBuffering stdout NoBuffering)
  ; liftIO (hSetEcho stdin False)
  ; gm <- get
  ; liftIO (gameDisplay gm >> hFlush stdout)
  ; liftIO (putStrLn (show (gsCurUnit gm))) -- for debug
  ; liftIO (putStrLn ("lockedp : "++show (gsLocked gm))) -- for debug
  ; loop
  }
  where
    loop = do
      { Right cmd <- liftIO (hGetCommand stdin)
      ; modify (gameStep cmd)
      ; gm <- get
      ; liftIO (gameDisplay gm >> hFlush stdout)
      ; liftIO (putStrLn (show (gsCurUnit gm))) -- for debug
      ; liftIO (putStrLn ("lockedp : "++show (gsLocked gm))) -- for debug
      ; if gsOver gm then dumpOutput gm else loop
      }

dumpOutput :: GameState -> Game ()
dumpOutput gm = return ()

hGetCommand :: Handle -> IO (Either Meta Command)
hGetCommand h =  return . keyToCommand =<< hGetChar h

keyToCommand :: Char -> Either Meta Command
keyToCommand 'h' = Right (Move W)
keyToCommand 'l' = Right (Move E)
keyToCommand 'j' = Right (Move SW)
keyToCommand 'k' = Right (Move SE)
keyToCommand ' ' = Right (Turn CW)
keyToCommand 'z' = Right (Turn CCW)
keyToCommand 'q' = Left Quit
keyToCommand 'd' = Left Dump
keyToCommand _   = Left None
