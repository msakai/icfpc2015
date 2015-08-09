import System.Environment
import Play
import qualified Tactics.RandomWalk as RandomWalk
import qualified Tactics.RandomZigZag as RandomZigZag    

main = do
  args <- getArgs
  case args of
    [problemid, cycle, playerName] -> do
      player <-
        case playerName of
          "RandomWalk" -> RandomWalk.newPlayer
          "RandomZigZag" -> RandomZigZag.newPlayer
          _ -> error "unknown player name"
      burst (read problemid) (read cycle) player
    _ -> error "USAGE: burst problemId cycle player"
