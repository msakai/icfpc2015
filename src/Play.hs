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
      { cmd <- liftIO (hGetCommand stdin)
      ; modify (gameStep cmd)
      ; gm <- get
      ; liftIO (gameDisplay gm >> hFlush stdout)
      ; liftIO (putStrLn (show (gsCurUnit gm))) -- for debug
      ; liftIO (putStrLn ("lockedp : "++show (gsLocked gm))) -- for debug
      ; if gsOver gm then dumpOutput gm else loop
      }

dumpOutput :: GameState -> Game ()
dumpOutput gm = return ()

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
