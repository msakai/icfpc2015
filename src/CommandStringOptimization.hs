module CommandStringOptimization
  ( optimize
  ) where

import Control.Monad
import Data.Char
import Data.IORef
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Command

import qualified ToySolver.SAT as SAT
import qualified ToySolver.SAT.Types as SAT
import qualified ToySolver.SAT.PBO as PBO

optimize :: [String] -> Command.Commands -> IO (String, Int)
optimize ps cmds = do
  let len = length cmds
  solver <- SAT.newSolver

  ms <- forM cmds $ \cmd -> do
     let cs = Command.commandToChar cmd
     m <- liftM Map.fromList $ forM cs $ \c -> do
       v <- SAT.newVar solver
       return (c,v)
     SAT.addExactly solver (Map.elems m) 1
     return m

  objRef <- newIORef []
  forM_ (map (map toLower) ps) $ \p -> do
    let p_len = length p
    unless (p_len > len) $ do
      ts <- liftM concat $ forM (take (len - p_len + 1) $ map (take p_len) $ tails ms) $ \ms2 -> do
        case sequence (zipWith Map.lookup p ms2) of
          Nothing -> return []
          Just vs -> do
            v <- SAT.newVar solver
            forM_ vs $ \v2 -> SAT.addClause solver [-v, v2] -- v implies v2
            modifyIORef objRef ((fromIntegral p_len * 2, v) :)
            return [v]
      p_appeard <- SAT.newVar solver
      SAT.addClause solver $ (- p_appeard) : ts -- p_appeard implies disjunction of ts
      modifyIORef objRef ((300, p_appeard) :)
      return ()

  obj' <- readIORef objRef
  let obj = [(-c,v) | (c,v) <- obj'] -- convert maximization to minimization
  opt <- PBO.newOptimizer solver obj
  PBO.setSearchStrategy opt PBO.BCD2
  PBO.setOnUpdateBestSolution opt $ \_ val -> print val
  PBO.optimize opt
  Just (model, val) <- PBO.getBestSolution opt

  let s = [head [c | (c,v) <- Map.toList m, SAT.evalLit model v] | m <- ms]
      v = - fromIntegral val
  return (s, v)
