module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid

data Astate = Astate { regCounter :: Int, ins :: Endo [String]}

type Analyzer a = State Astate a

-- O(1) time complexity
-- Idea from https://kseo.github.io/posts/2017-01-21-writer-monad.html
output :: String -> Analyzer ()
output x
  = do
      st <- get
      put st{ins = (( (ins st) <> (Endo ([x]<>))  )) }
      return ()




codeGenerator :: Int -> Analyzer [Char]
codeGenerator 0
  = do 
      output "11111\n"
      return "aaaa"

codeGenerator n
  = do
      output "11111\n"
      x <- codeGenerator (n-1)
      return x


test :: IO ()
test
  = do
      let
        state = Astate
          { regCounter = 0, ins = Endo ([]<>) }
      let (r, s) = runState (codeGenerator 100000) state
      putStrLn r
      putStrLn (concat (appEndo (ins s) []))
      return ()
  