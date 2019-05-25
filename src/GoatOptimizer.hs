module GoatOptimizer where

import OzInstruction

removeComments :: [Instruction] -> [Instruction]
removeComments ins = filter (not . isComment) ins

isComment :: Instruction -> Bool
isComment (IComment _) = True
isComment _ = False

runOptimizer :: [Instruction] -> [Instruction]
runOptimizer ins
  = optimizer1 ins 

-- remove case like: Store 1, r0  Load r0, 1
optimizer1 :: [Instruction] -> [Instruction]
optimizer1 ins = optimizer1_ (IComment "placeholder") ins

optimizer1_ :: Instruction -> [Instruction] -> [Instruction]
optimizer1_ _ [] = []
optimizer1_ (IStatement (Store slot0 reg0)) ((IStatement (Load reg1 slot1)):xs)
  | slot0 == slot1 && reg0 == reg1 = optimizer1_ (IStatement (Store slot0 reg0)) xs
  | otherwise = (IStatement (Load reg1 slot1)):(optimizer1_ (IStatement (Load reg1 slot1)) xs)
optimizer1_ last ((IComment x):xs) -- skip comments
  = (IComment x):(optimizer1_ last xs)
optimizer1_ _ (x:xs)
  = x:(optimizer1_ x xs)
