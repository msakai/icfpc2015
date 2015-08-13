import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit

import Types
import Game hiding (id)
import qualified Game
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
  { optInputs :: [FilePath]
  , optTimeLimit :: Maybe Int
  , optMemoryLimit :: Maybe Int
  , optNCores :: Maybe Int
  , optPhrases :: [String]
  , optHelp :: Bool
  }

defaultOptions :: Options
defaultOptions
  = Options
  { optInputs = []
  , optTimeLimit = Nothing
  , optMemoryLimit = Nothing
  , optNCores = Nothing
  , optPhrases = []
  , optHelp = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "f" [] (ReqArg (\val opt -> opt{ optInputs = val : optInputs opt }) "FILENAME") "File containing JSON encoded input"
  , Option "t" [] (ReqArg (\val opt -> opt{ optTimeLimit = Just (read val) }) "NUMBER") "Time limit, in seconds, to produce output"
  , Option "m" [] (ReqArg (\val opt -> opt{ optMemoryLimit = Just (read val) }) "NUMBER") "Memory limit, in megabytes, to produce output"
  , Option "c" [] (ReqArg (\val opt -> opt{ optNCores = Just (read val) }) "NUMBER") "Number of processor cores available"
  , Option "p" [] (ReqArg (\val opt -> opt{ optPhrases = val : optPhrases opt }) "STRING") "Phrase of power"
  , Option "h" ["help"] (NoArg (\opt -> opt{ optHelp = True }) ) "Print help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (_,_,errs@(_:_)) -> do
      mapM_ putStrLn errs
      exitFailure

    (o,args2,[]) -> do
      let opt = foldl (flip id) defaultOptions o
      if optHelp opt then do
        help
      else do
        player <- Hybrid.newPlayer 300
        os <- liftM concat $ forM (optInputs opt) $ \fname -> do
          Just input <- readProblem fname
          forM (zip (initGameStates input (optPhrases opt)) [0..]) $ \(gm, sd) -> do
            let gm2 = autoPlay2 player gm
            tag <- genTag (Game.id input) sd
            return $ dumpOutputItem gm2 tag
        LBS.putStrLn $ encode os

    _ -> help

help :: IO ()
help = putStrLn $ usageInfo "USAGE: davar [OPTIONS] -p STRING -p STRING .. -f FILENAME -f FILENAME .." options
