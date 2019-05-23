module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid
import OzInstruction
import GoatAST


data Gstate = Gstate { regCounter :: Int, instructions :: Endo [Instruction]}

type Analyzer a = State Gstate a

-- O(1) time complexity
-- Idea from https://kseo.github.io/posts/2017-01-21-writer-monad.html
appendIns :: Instruction -> Analyzer ()
appendIns x
  = do
      st <- get
      put st{instructions = (( (instructions st) <> (Endo ([x]<>))  )) }

----------------------------------------------

genProgram :: DGoatProgram -> Analyzer ()
genProgram _ 
  = do 
      appendIns (Comment "AAAA")

runCodeGenerator :: DGoatProgram -> [Instruction]
runCodeGenerator dGoatProgram
  = let state = Gstate { regCounter = 0, instructions = Endo ([]<>) }
        (_, s) = runState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []

test = runCodeGenerator (DProgram 0 [])
