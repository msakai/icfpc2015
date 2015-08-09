import System.Environment
import Play

main = do
  args <- getArgs
  case args of
    [problemid, seedno] -> do
      testPlay (read problemid) (read seedno) play
    _ -> error "USAGE: play problemId seedNo"
