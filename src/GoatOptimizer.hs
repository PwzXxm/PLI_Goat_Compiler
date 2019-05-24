module GoatOptimizer where

import OzInstruction

removeComments :: [Instruction] -> [Instruction]
removeComments ins = filter (not . isComment) ins

isComment :: Instruction -> Bool
isComment (IComment _) = True
isComment _ = False

runOptimizer :: [Instruction] -> [Instruction]
runOptimizer ins = ins 
