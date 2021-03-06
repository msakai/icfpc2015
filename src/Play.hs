{-# LANGUAGE
    BangPatterns
  #-}
module Play
  ( genTag

  , DisplayMode (..)
  , autoPlay
  , autoPlay2
  , autoPlay3
  , runGame

  , play
  , playStartWith

  , PlayerM (..)
  , Player
  , getGameState
  , command
  , queryPlayer
  , replayPlayer
  ) where

import Control.Applicative
import Control.Arrow ((&&&))    
import Control.Concurrent.Thread.Delay (delay)
import Control.Exception
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

initGameIO :: ProblemId -> SeedNo -> IO GameState
initGameIO n sd = do
  { input <- readProblem ("problems/problem_"++show n++".json")
  ; case input of { Nothing -> error "not found the seed"; Just inp -> return (initGameStates inp phrases !! sd) }
  }

genTag :: ProblemId -> SeedNo -> IO Tag
genTag n sd = do
  now <- getCurrentTime
  zone <- getCurrentTimeZone
  let (TimeOfDay h m s) = localTimeOfDay $ utcToLocalTime zone now
  return $ "problem" ++ show n ++ "-seed" ++ show sd ++ "-"++ show h ++ "-" ++ show m ++ "-" ++ show s

data DisplayMode
  = DisplayAll
  | DisplayLocked
  | DisplayNone
  deriving (Show, Eq, Ord, Enum, Bounded)

autoPlay :: ProblemId -> SeedNo -> DisplayMode -> Double -> Player -> IO ()
autoPlay n sd displayMode wait_s player = runGame n sd $ autoPlay' displayMode wait_s player
           
autoPlay' :: DisplayMode -> Double -> Player -> Game ()
autoPlay' displayMode wait_s player = loop True player
  where
    wait = do
      when (wait_s > 0) $ liftIO $ do
        let us = ceiling (wait_s  * 10^(6::Int))
        delay us

    loop first player = do
      gm <- get
      case displayMode of
        DisplayAll -> do
          liftIO $ gameDisplay' gm
          wait
        DisplayLocked -> do
          when (gsStatus gm /= Game.Running || first || gsLocked gm) $ do
            liftIO $ gameDisplay' gm
            wait
        DisplayNone -> return ()
      case gsStatus gm of
        Running -> do
          case queryPlayer gm player of
            (!c, player') -> do
              put (gameStep c gm)
              loop False player'
        _ -> do
          liftIO $ putStrLn "QUIT"
          gm <- get
          let cmds = reverse $ gsCommands gm
          liftIO $ print $ gsScore gm
          liftIO $ print $ cmds
          liftIO $ print $ head $ commandsToString cmds

autoPlay2 :: Player -> GameState -> GameState
autoPlay2 !player !gm
  | gsStatus gm /= Running = gm
  | otherwise =
      case queryPlayer gm player of
        (!c, player') -> do
          autoPlay2 player' (gameStep c gm)

autoPlay3 :: DisplayMode -> Double -> Player -> GameState -> IO GameState
autoPlay3 displayMode wait_s player gm = execStateT (autoPlay' displayMode wait_s player) gm

runGame :: ProblemId -> SeedNo -> Game () -> IO ()
runGame n sd player = do
  tag <- liftIO (genTag n sd)
  gms <- initGameIO n sd
  gms' <- execStateT player gms
  let filename = "outputs/"++show (gsScore gms')++"pt-"++tag++".json"
  LBS.writeFile filename $ encode [ dumpOutputItem gms' tag ]
  return ()

play :: Game ()
play = playStartWith []
           
playStartWith :: [Command] -> Game ()
playStartWith cmds = do
  { liftIO (hSetBuffering stdin NoBuffering)
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
      putStr prompt
      hFlush stdout
      bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
        bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
          hSetBuffering stdin LineBuffering
          hSetEcho stdin True
          getLine

hGetCommand :: Handle -> IO (Either Meta Command)
hGetCommand h =  liftM keyToCommand $ hGetChar h

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

queryPlayer :: GameState -> Player -> (Command, Player)
queryPlayer gs (PReturn _) = error "stepPlayer: should not happen"
queryPlayer gs (PCommand o cont) = (o, cont)
queryPlayer gs (PGetGameState cont) = queryPlayer gs (cont gs)

replayPlayer :: [Command] -> Player
replayPlayer = mapM_ command                
