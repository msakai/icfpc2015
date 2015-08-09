import System.Environment
import Play
import qualified Tactics.RandomWalk as RandomWalk
import qualified Tactics.RandomWalk2 as RandomWalk2
import qualified Tactics.RandomZigZag as RandomZigZag    

main = do
  args <- getArgs
  case args of
    [problemid, cycle, playerName] -> do
      let player =
            case playerName of
              "RandomWalk" -> RandomWalk.newPlayer
              "RandomWalk2" -> RandomWalk2.newPlayer
              "RandomZigZag" -> RandomZigZag.newPlayer
              _ -> error "unknown player name"
      burst (read problemid) (read cycle) player
    _ -> error "USAGE: burst problemId cycle player"
