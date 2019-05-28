-- | Optimizer for Oz instructions that generated from Goat
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatOptimizer(
  runOptimizer,
  removeComments
) where

import OzInstruction

-- | remove all comment instructions
removeComments :: [Instruction] -> [Instruction]
removeComments ins = filter (not . isComment) ins

-- | optimize the given list of instructions
runOptimizer :: [Instruction] -> [Instruction]
runOptimizer insList
  = backwardScanner $ forwardScanner insList

-------------
--  Private 
-------------

-- | return True if the given instruction is a comment
isComment :: Instruction -> Bool
isComment (IComment _) = True
isComment _ = False

-- | run the forward scanner (update an insturction based on the previous one)
forwardScanner :: [Instruction] -> [Instruction]
forwardScanner insList
  = forwardScanner_ (IComment "placeholder") insList

-- | the actual forward scanner 
--   lastInstruction -> remainingInstructions
forwardScanner_ :: Instruction -> [Instruction] -> [Instruction]
forwardScanner_ lastIns (ins:insList)
  = let 
      newLast = case ins of 
        -- do not update the lastIns with a comment
        (IComment _) -> lastIns
        _            -> ins
      -- (previous, current)
      listPair = (lastIns, ins)
      recursion = forwardScanner_ newLast insList
    in
      case listPair of
        -- Store s0 r0; Load r0 s0 -> Store s0 r0
        -- Store s0 r0; Load r1 s0 -> Store s0 r0; Move r1 r0
        (IStatement (Store slot0 reg0), IStatement (Load reg1 slot1)) | slot0 == slot1
          -> if reg0 == reg1 then recursion else ((IOperation (Move reg1 reg0)):recursion)
        _
          -> (ins:recursion)

forwardScanner_ _ [] = []


-- | run the backward scanner (update an insturction based on the next one)
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

-- | get next actual instruction (skip comment)
nextActualIns :: [Instruction] -> Instruction
nextActualIns (ins:insList)
  | (isComment ins) == False = ins
  | otherwise = nextActualIns insList
nextActualIns [] = IComment "placeholder"
