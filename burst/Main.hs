import Data.Char
import System.Environment
import Play
import qualified Tactics.RandomWalk as RandomWalk
import qualified Tactics.RandomWalk2 as RandomWalk2
import qualified Tactics.RandomZigZag as RandomZigZag
import qualified Tactics.Reynolds as Reynolds

main = do
  args <- getArgs
  case args of
    [problemid, cycle, playerName] -> do
      let newPlayer =
            case map toLower playerName of
              "randomwalk" -> RandomWalk.newPlayer
              "randomwalk2" -> RandomWalk2.newPlayer
              "randomzigzag" -> RandomZigZag.newPlayer
              "reynolds" -> Reynolds.newPlayer 300
              _ -> error "unknown player name"
      burst (read problemid) (read cycle) newPlayer
    _ -> error "USAGE: burst problemId cycle player"
