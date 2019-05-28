module GoatOptimizer where

import Control.Monad.State
import           Data.Monoid

import OzInstruction

removeComments :: [Instruction] -> [Instruction]
removeComments ins = filter (not . isComment) ins

isComment :: Instruction -> Bool
isComment (IComment _) = True
isComment _ = False

runOptimizer :: [Instruction] -> [Instruction]
runOptimizer insList
  = let state = Ostate { lastIns = IComment "placeholder", instructions = Endo ([]<>) }
        s = execState (mainOptimizer insList) state
    in backwardScanner ((appEndo (instructions s)) [])

data Ostate = Ostate 
  { lastIns :: Instruction, instructions :: Endo [Instruction]}

type Optimizer a = State Ostate a

-- O(1) time complexity
-- Idea from https://kseo.github.io/posts/2017-01-21-writer-monad.html
appendIns :: Instruction -> Optimizer ()
appendIns x
  = do
      st <- get
      put st{instructions = (( (instructions st) <> (Endo ([x]<>))  )) }

getLastIns :: Optimizer Instruction
getLastIns
  = do
      st <- get
      return (lastIns st)

updateLastIns :: Instruction -> Optimizer ()
updateLastIns ins
  = case ins of
      -- ignore comments
      (IComment _) -> return ()
      ins -> do
        st <- get
        put st{lastIns = ins}


mainOptimizer :: [Instruction] -> Optimizer ()
mainOptimizer insList0
  = do
      mapM_ forwardScanner insList0
    
forwardScanner :: Instruction -> Optimizer ()
forwardScanner ins
  = do
      last <- getLastIns
      updateLastIns ins

      -- (last ins, current ins)
      let insPair = (last, ins)
      case insPair of
        (IStatement (Store slot0 reg0), IStatement (Load reg1 slot1)) | slot0 == slot1
          -> if reg0 == reg1
              then return ()
              else appendIns (IOperation (Move reg1 reg0))
        _
          -> appendIns ins


backwardScanner :: [Instruction] -> [Instruction]
backwardScanner (ins:insList)
  -- (current, next)
  = let insPair = (ins, nextActualIns insList) in
    case insPair of 
      (IBranch (Uncond label0), ILabel label1) | label0 == label1
        -> backwardScanner insList
      _
        -> (ins:backwardScanner insList)

backwardScanner [] = []

nextActualIns :: [Instruction] -> Instruction
nextActualIns (ins:insList)
  | (isComment ins) == True = ins
  | otherwise = nextActualIns insList
nextActualIns [] = IComment "placeholder"
