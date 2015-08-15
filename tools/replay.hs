import Control.Monad
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.Console.GetOpt
import System.Environment
import System.Exit

import Command
import Types
import Game hiding (id)
import qualified Game
import Play
import Util

data Options
  = Options
  { optPhrases :: [String]
  , optDisplayMode :: DisplayMode
  , optWaitSec :: Double                  
  , optHelp :: Bool
  }

defaultOptions :: Options
defaultOptions
  = Options
  { optPhrases = []
  , optDisplayMode = DisplayAll
  , optWaitSec = 0
  , optHelp = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "p" [] (ReqArg (\val opt -> opt{ optPhrases = val : optPhrases opt }) "STRING") "Phrase of power"
  , Option [] ["all-phrases"] (NoArg (\opt -> opt{ optPhrases = phrases ++ optPhrases opt }) ) "Enable all phrases"
  , Option [] ["display-mode"] (ReqArg (\val opt -> opt{ optDisplayMode = parseDisplayMode val }) "str") "display mode: all, locked, none"
  , Option [] ["wait"] (ReqArg (\val opt -> opt{ optWaitSec = read val }) "double") "wait (in sec)"
  , Option "h" ["help"] (NoArg (\opt -> opt{ optHelp = True }) ) "Print help message"
  ]
  where
    parseDisplayMode "all" = DisplayAll
    parseDisplayMode "locked" = DisplayLocked
    parseDisplayMode "none" = DisplayNone
    parseDisplayMode s = error $ "unknown display mode: " ++ s

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (_,_,errs@(_:_)) -> do
      mapM_ putStrLn errs
      exitFailure

    (o,args2,[]) -> do
        let opt = foldl (flip id) defaultOptions o
        forM_ args2 $ \fname -> do
          Just output <- readOutput fname
          forM_ output $ \o -> do
            let probId = Game.problemId o
            Just input  <- Util.readProblem $ "problems/problem_" ++ show (Game.problemId o) ++ ".json"
            let gm = initGameState input (optPhrases opt) (Game.seed o)
            let player = replayPlayer $ stringToCommands $ (Game.solution o)
            _ <- autoPlay3 (optDisplayMode opt) (optWaitSec opt) player gm
            return ()

    _ -> help

help :: IO ()
help = putStrLn $ usageInfo "USAGE: replay [OPTIONS] -p STRING -p STRING .. -f FILENAME -f FILENAME .. FILENAME .." options
