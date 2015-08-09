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

data Meta = Nop | Dump | Quit | BatchH | BatchS deriving (Show, Eq)

initGameIO :: ProblemId -> Tag -> SeedNo -> IO GameState
initGameIO n tg sd = do
  { input <- readProblem ("problems/problem_"++show n++".json")
  ; case input of { Nothing -> error "not found the seed"; Just inp -> return (initGameStates tg inp !! sd) }
  }

genTag :: ProblemId -> SeedNo -> IO Tag
genTag n sd = do
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  let (TimeOfDay h m s) = localTimeOfDay $ utcToLocalTime zone now
  return $ "problem" ++ show n ++ "-seed[" ++ show sd ++ "]-"++ show h ++ "-" ++ show m ++ "-" ++ show s

burst :: ProblemId -> Int -> IO Player -> IO ()
burst pid cyc ani = do
  Just inp <- readProblem ("problems/problem_"++show pid++".json")
  let sdN = length $ sourceSeeds inp
  mapM_ (\(n,sd) -> autoPlay pid sd =<< ani) [(n, sd) | n <- [1..cyc], sd <- [0..sdN-1]]

autoPlay :: ProblemId -> SeedNo -> Player -> IO ()
autoPlay n sd ani = testPlay n sd (play' 0 ani)

testPlay :: ProblemId -> SeedNo -> Game () -> IO ()
testPlay n sd player = do
  tag <- liftIO (genTag n sd)
  gms <- initGameIO n tag sd
  gms' <- execStateT player gms
--  LBS.putStrLn $ encode $ dumpOutputItem gms'
  let filename = "outputs/problem"++show n++
                 "-seed"++show (gsSeed gms')++
                 "-"++show (gsScore gms')++"pt-"++
                 tag++".json"
  LBS.writeFile filename $ encode [ toJSON (dumpOutputItem gms') ]
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
play = playStartWith []
           
playStartWith :: [Command] -> Game ()
playStartWith cmds = do
  { liftIO (hSetBuffering stdin NoBuffering)
  ; liftIO (hSetBuffering stdout NoBuffering)
  ; liftIO (hSetEcho stdin False)
  ; modify (gameStepN cmds)
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
    opMeta BatchH = do
      s <- getLineWithPrompt "reading [Command] expression in haskell > "
      case reads s of
        (cmds,_) : _ -> do
          modify (gameStepN cmds)
          loop
        _ -> do
          liftIO $ putStrLn "parse error"
          loop
    opMeta BatchS = do
      s <- getLineWithPrompt "reading solution string > "
      modify (gameStepN (stringToCommands s))
      loop
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

    getLineWithPrompt prompt = liftIO $ do
      hSetBuffering stdin LineBuffering
      hSetBuffering stdout LineBuffering                      
      hSetEcho stdin True
      putStr prompt
      hFlush stdout
      s <- getLine
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetEcho stdin False
      return s

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
keyToCommand 'b' = Left BatchH
keyToCommand 's' = Left BatchS
keyToCommand _   = Left Nop

commandsToKeys :: [Command] -> String
commandsToKeys = fmap commandToKey

commandToKey :: Command -> Char
commandToKey (Move W)   = 'h'
commandToKey (Move E)   = 'l'
commandToKey (Move SW)  = 'j'
commandToKey (Move SE)  = 'k'
commandToKey (Turn CW)  = ' '
commandToKey (Turn CCW) = 'z'
    

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
