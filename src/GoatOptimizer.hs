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
  = backwardScanner $ forwardScanner insList

forwardScanner :: [Instruction] -> [Instruction]
forwardScanner insList
  = forwardScanner_ (IComment "placeholder") insList

forwardScanner_ :: Instruction -> [Instruction] -> [Instruction]
forwardScanner_ lastIns (ins:insList)
  = let 
      newLast = case ins of 
        (IComment _) -> lastIns
        _            -> ins
      -- (previous, current)
      listPair = (lastIns, ins)
      recursion = forwardScanner_ newLast insList
    in
      case listPair of
        (IStatement (Store slot0 reg0), IStatement (Load reg1 slot1)) | slot0 == slot1
          -> if reg0 == reg1 then recursion else ((IOperation (Move reg1 reg0)):recursion)
        _
          -> (ins:recursion)

forwardScanner_ _ [] = []


backwardScanner :: [Instruction] -> [Instruction]
backwardScanner (ins:insList)
  -- (current, next)
  = let 
      insPair = (ins, nextActualIns insList)
      recursion = backwardScanner insList
    in
      case insPair of 
        (IBranch (Uncond label0), ILabel label1) | label0 == label1
          -> recursion
        _
          -> (ins:recursion)

backwardScanner [] = []

nextActualIns :: [Instruction] -> Instruction
nextActualIns (ins:insList)
  | (isComment ins) == True = ins
  | otherwise = nextActualIns insList
nextActualIns [] = IComment "placeholder"
