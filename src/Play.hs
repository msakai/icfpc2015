module Play where

import Control.Arrow ((&&&))
import Control.Concurrent.Thread.Delay (delay)
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

data Meta = Nop | Dump | Quit deriving (Show, Eq)

initGameIO :: Int -> String-> IO [GameState]
initGameIO n tg = do
  { input <- readProblem ("problems/problem_"++show n++".json")
  ; case input of { Nothing -> return []; Just inp -> return (initGameStates tg inp) }
  }

testPlay :: Int -> Game () -> IO ()
testPlay n player = do
  gms <- initGameIO n "test-tag"
  runStateT player (head gms)
  return ()

play' :: (GameState -> IO Commands) -> Integer -> Game ()
play' ani wait = do
  { gm <- get
  ; liftIO $ gameDisplay gm
  ; ccs <- liftIO (ani gm)
  ; loop ccs
  }
  where
    quit = return ()
    loop (c:cs) = do
      { modify (gameStep c)
      ; gm <- get
      ; liftIO (gameDisplay gm >> delay wait)
      ; case gsStatus gm of
          Running -> loop cs
          _       -> quit
      }

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
    dump = do
      { gm <- get
      ; liftIO $ putStrLn $ show $ reverse $ gsCommands gm
      }
    quit = liftIO $ putStrLn "QUIT"
    opMeta Quit = dump >> quit
    opMeta Dump = dump >> loop
    opMeta Nop  = loop
    opCommand cmd = do
      { modify (gameStep cmd)
      ; gm <- get
      ; liftIO (gameDisplay gm >> hFlush stdout)
      ; liftIO (putStrLn (show (gsCurUnit gm))) -- for debug
      ; liftIO (putStrLn ("lockedp : "++show (gsLocked gm))) -- for debug
      ; case gsStatus gm of 
          Running -> loop
          _       -> quit
      }
    loop = do
      { cmd <- liftIO (hGetCommand stdin)
      ; either opMeta opCommand cmd
      }

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
keyToCommand _   = Left Nop
