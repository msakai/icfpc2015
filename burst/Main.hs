import Control.Monad
import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit

import Types
import Game hiding (id)
import Play
import Util
import qualified Tactics.RandomWalk as RandomWalk
import qualified Tactics.RandomWalk2 as RandomWalk2
import qualified Tactics.RandomZigZag as RandomZigZag
import qualified Tactics.Reynolds as Reynolds
import qualified Tactics.Hybrid as Hybrid
import qualified Tactics.SearchAllLocking as SearchAllLocking

data Options
  = Options
  { optDisplayMode :: DisplayMode
  , optWaitSec :: Double
  , optSeedIdxs :: [Int]
  , optPlayer :: String
  , optRepeatNum :: Int
  , optReynoldsN :: Int                   
  }

defaultOptions :: Options
defaultOptions
  = Options
  { optDisplayMode = DisplayAll
  , optSeedIdxs    = []
  , optWaitSec     = 0
  , optPlayer      = "reynolds"
  , optRepeatNum   = 1
  , optReynoldsN   = 300
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["display-mode"] (ReqArg (\val opt -> opt{ optDisplayMode = parseDisplayMode val }) "str") "display mode: all, locked, none"
  , Option [] ["wait"] (ReqArg (\val opt -> opt{ optWaitSec = read val }) "double") "wait (in sec)"
  , Option [] ["seed"] (ReqArg (\val opt -> opt{ optSeedIdxs = read val : optSeedIdxs opt }) "int") "seed index (multiple options allowed)"
  , Option [] ["player"] (ReqArg (\val opt -> opt{ optPlayer = val }) "str") "player name"
  , Option [] ["repeat"] (ReqArg (\val opt -> opt{ optRepeatNum = read val }) "int") "number of repetition (default: 1)"
  , Option [] ["reynolds-n"] (ReqArg (\val opt -> opt{ optReynoldsN = read val }) "int") ("(default: " ++  show (optReynoldsN defaultOptions) ++ ")")
  ]
  where
    parseDisplayMode "all" = DisplayAll
    parseDisplayMode "locked" = DisplayLocked
    parseDisplayMode "none" = DisplayNone
    parseDisplayMode s = error $ "unknown display mode: " ++ s
    
main = do
  args <- getArgs
  case getOpt Permute options args of
    (_,_,errs@(_:_)) -> do
      mapM_ putStrLn errs
      exitFailure

    (o,args2,[]) -> do
      let opt = foldl (flip id) defaultOptions o
      let newPlayer =
            case map toLower (optPlayer opt) of
              "randomwalk" -> RandomWalk.newPlayer
              "randomwalk2" -> RandomWalk2.newPlayer
              "randomzigzag" -> RandomZigZag.newPlayer
              "reynolds" -> Reynolds.newPlayer (optReynoldsN opt)
              "hybrid" -> Hybrid.newPlayer (optReynoldsN opt)                            
              "searchalllocking" -> return SearchAllLocking.player
              _ -> error "unknown player name"
      if null args2 then
        help
      else do
        forM_ args2 $ \problemid -> do
          burst (read problemid) (optRepeatNum opt) (optSeedIdxs opt) (optDisplayMode opt) (optWaitSec opt) newPlayer

    _ -> help

help :: IO ()
help = putStrLn $ usageInfo "USAGE: burst [OPTIONS] problemId1 problemId2 .." options

burst :: ProblemId -> Int -> [Int] -> DisplayMode -> Double -> IO Player -> IO ()
burst pid cyc seedIdxs displayMode wait_s newPlayer = do
  Just inp <- readProblem ("problems/problem_"++show pid++".json")
  let sdN = length $ sourceSeeds inp
  forM_ [1..cyc] $ \n -> do
    if null seedIdxs then do
      forM_ [0..sdN-1] $ \sd -> do
        player <- newPlayer
        autoPlay pid sd displayMode wait_s player
    else
      forM_ seedIdxs $ \sd -> do
        player <- newPlayer
        autoPlay pid sd displayMode wait_s player
