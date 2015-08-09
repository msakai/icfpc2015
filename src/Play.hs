module Play where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Concurrent.Thread.Delay (delay)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef
import Data.Maybe (isJust, isNothing, maybe, fromJust)
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO
import Text.PrettyPrint.Boxes (printBox)

import Command
import Game
import Display
import Unit
import Util
import Types

data Meta = Nop | Dump | Quit deriving (Show, Eq)

initGameIO :: ProblemId -> Tag -> IO [GameState]
initGameIO n tg = do
  { input <- readProblem ("problems/problem_"++show n++".json")
  ; case input of { Nothing -> return []; Just inp -> return (initGameStates tg inp) }
  }

genTag :: ProblemId -> IO Tag
genTag n = do
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  let (TimeOfDay h m s) = localTimeOfDay $ utcToLocalTime zone now
  return $ "problem" ++ show n ++ "-" ++ show h ++ "-" ++ show m ++ "-" ++ show s

autoPlay :: ProblemId -> Player -> IO ()
autoPlay n ani = testPlay n (play' 0 ani)

testPlay :: ProblemId -> Game () -> IO ()
testPlay n player = do
  tag <- liftIO (genTag n)
  gms <- initGameIO n tag
  gms' <- execStateT player (head gms)
--  LBS.putStrLn $ encode $ dumpOutputItem gms'
  let filename = "problem"++show n++"-"++tag++"-score"++show (gsScore gms')++".json"
  handle <- openFile filename WriteMode
  hPutStr handle $ LBS.unpack $ encode $ dumpOutputItem gms'
  hClose handle
  return ()

play' :: WaituS -> Player -> Game ()
play' wait = loop
  where
    loop ani = do
      gm <- get
      liftIO (gameDisplay' gm >> delay wait)
      case gsStatus gm of
        Running -> do
          let (c, ani') = runPlayer gm ani
          put (gameStep c gm)
          loop ani'
        _       -> quit
    dump = do
      { gm <- get
      ; let cmds = reverse $ gsCommands gm
      ; liftIO $ print $ cmds
      ; liftIO $ print $ head $ commandsToString cmds
      }
    quit = do
      { liftIO $ putStrLn "QUIT"
      ; dump
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
      ; let cmds = reverse $ gsCommands gm
      ; liftIO $ print $ cmds
      ; liftIO $ print $ head $ commandsToString cmds
      }
    quit = do
      { liftIO $ putStrLn "QUIT"
      ; dump
      }
    opMeta Quit = quit
    opMeta Dump = dump >> loop
    opMeta Nop  = loop
    opCommand cmd = modify (gameStep cmd) >> loop
    loop = do
      { gm <- get
      ; liftIO (gameDisplay' gm >> hFlush stdout)
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


data PlayerM a
  = PReturn a
  | PCommand Command (PlayerM a)
  | PGetGameState (GameState -> PlayerM a)

instance Functor PlayerM where
  fmap = liftM

instance Applicative PlayerM where
  pure  = return
  (<*>) = ap

instance Monad PlayerM where
  return = PReturn
  PReturn x >>= f = f x
  PCommand o cont >>= f = PCommand o (cont >>= f)
  PGetGameState cont >>= f = PGetGameState (\i -> cont i >>= f)

type Player = PlayerM ()

getGameState :: PlayerM GameState
getGameState = PGetGameState return

command :: Command -> Player
command x = PCommand x (return ())

runPlayer :: GameState -> Player -> (Command, Player)
runPlayer gs (PReturn _) = error "stepPlayer: should not happen"
runPlayer gs (PCommand o cont) = (o, cont)
runPlayer gs (PGetGameState cont) = runPlayer gs (cont gs)

replayPlayer :: [Command] -> Player
replayPlayer = mapM_ command                
