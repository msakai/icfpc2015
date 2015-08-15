
import Control.Monad
import Data.List.Split
import System.FilePath
import qualified System.FilePath.Glob as Glob
import System.IO
import System.Process

import Types
import qualified Board    
import Command
import qualified Cell
import qualified Game
import qualified PRNG
import qualified Score
import qualified Unit
import qualified Util
    
main :: IO ()
main = do
  fnames <- Glob.glob "outputs/*pt-*.json"
  forM_ fnames $ \fpath -> do
    let (dirname, fname) = splitFileName fpath
        (score' : _ : seed' : _) = splitOn "-" fname
        score :: Int
        score = read $ reverse $ drop 2 $ reverse score'
        idx :: Int
        idx = read (drop 4 seed')

    Just [o] <- Util.readOutput (dirname </> fname)
    Just input  <- Util.readProblem $ "problems/problem_" ++ show (Game.problemId o) ++ ".json"
    let gs0 = Game.initGameState input phrases (Game.seed o)
        gs  = Game.gameStepN (stringToCommands (Game.solution o)) gs0
        new_score = Game.gsMScore gs + Score.computePowerScoreFromString phrases (Game.solution o)
        fpath2 = dirname </> (show new_score ++ "pt" ++ dropWhile ('-'/=) fname)
    
    -- putStrLn $ fname ++ ": " ++ show score ++ " -> " ++ show new_score
    when (score /= new_score) $ do
      putStrLn $ fpath ++ " -> " ++ fpath2
      callProcess "git" ["mv", fpath, fpath2]


