module GoatFormatter where

import GoatAST

runGoatFormatter :: GoatProgram -> IO ()
runGoatFormatter (Program (x:xs))
  = do
      putStrLn (show x)
      runGoatFormatter (Program xs)

runGoatFormatter (Program [])
  = do
      return ()