import Control.Monad
import Data.Char
import System.Console.GetOpt
import System.Environment
import System.Exit
    
import Play
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
  , optPlayer :: String
  , optRepeatNum :: Int
  , optReynoldsN :: Int                   
  }

defaultOptions :: Options
defaultOptions
  = Options
  { optDisplayMode = DisplayAll
  , optWaitSec     = 0
  , optPlayer      = "reynolds"
  , optRepeatNum   = 1
  , optReynoldsN   = 300
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option [] ["display-mode"] (ReqArg (\val opt -> opt{ optDisplayMode = parseDisplayMode val }) "str") "display mode: all, locked, none"
  , Option [] ["wait"] (ReqArg (\val opt -> opt{ optWaitSec = read val }) "double") "wait (in sec)"
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
          burst (read problemid) (optRepeatNum opt) (optDisplayMode opt) (optWaitSec opt) newPlayer

    _ -> help

help = putStrLn $ usageInfo "USAGE: burst [OPTIONS] problemId1 problemId2" options
