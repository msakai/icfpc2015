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

play' :: (GameState -> IO Commands) -> Game ()
play' ani = do
  { gm <- get
  ; liftIO $ gameDisplay gm
  ; loop gm
  }
  where
    quit = return ()
    loop gm = do
      { (c:cs) <- liftIO (ani gm)
      ; modify (gameStep c)
      ; gm <- get
      ; liftIO (gameDisplay gm)
      ; case gsStatus gm of
          Running -> loop gm
          _       -> quit
      }

play :: Game ()
play = do 
  { liftIO (hSetBuffering stdin NoBuffering)
  ; liftIO (hSetBuffering stdout NoBuffering)
  ; liftIO (hSetEcho stdin False)
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
    opCommand cmd = modify (gameStep cmd) >> loop
    loop = do
      { gm <- get
      ; liftIO (gameDisplay gm >> hFlush stdout)
      ; liftIO (putStrLn (show (gsCurUnit gm))) -- for debug
      ; liftIO (putStrLn ("lockedp : "++show (gsLocked gm))) -- for debug
      ; case gsStatus gm of
          Running -> do
           { cmd <- liftIO (hGetCommand stdin)
           ; either opMeta opCommand cmd
           }
          _       -> quit
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
