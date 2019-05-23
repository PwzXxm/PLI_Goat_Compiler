module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid
import OzInstruction
import GoatAST


data Gstate = Gstate { regCounter :: Int, instructions :: Endo [Instruction]}

type Generator a = State Gstate a

-- O(1) time complexity
-- Idea from https://kseo.github.io/posts/2017-01-21-writer-monad.html
appendIns :: Instruction -> Generator ()
appendIns x
  = do
      st <- get
      put st{instructions = (( (instructions st) <> (Endo ([x]<>))  )) }

getReg :: Generator Int
getReg
  = do
      st <- get
      put st{regCounter = (regCounter st) + 1}
      return (regCounter st)

setNextUnusedReg :: Int -> Generator ()
setNextUnusedReg n
  = do
      st <- get
      put st{regCounter = n}

----------------------------------------------

genProgram :: DGoatProgram -> Generator ()
genProgram (DProgram mainId dProcs)
  = do 
      appendIns (ICall $ "proc_" ++ (show mainId))
      appendIns (IHalt)
      mapM_ genProc dProcs

genProc :: DProc -> Generator ()
genProc (DProc procId dParas dStmts slotSize)
  = do
      appendIns (ILabel $ "proc_" ++ (show procId))
      appendIns (IComment $ "code for procedure " ++ (show procId))
      appendIns (IPushStack slotSize)
      -- TODO: handle parameters
      appendIns (IComment $ "init variable")
      reg0 <- getReg
      -- init value
      appendIns (IConstant $ ConsInt reg0 0)
      mapM_ (\i -> do appendIns (IStatement $ Store i reg0)) [0..(slotSize-1)]
      setNextUnusedReg reg0



      appendIns (IPopStack slotSize)
      appendIns (IReturn)





runCodeGenerator :: DGoatProgram -> [Instruction]
runCodeGenerator dGoatProgram
  = let state = Gstate { regCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []

test 
  = do 
      let ins = runCodeGenerator (DProgram 0 [DProc 0 [] [DRead (DVar 0 ShapeVar IntType),DCall 1 [DEvar (DVar 0 ShapeVar IntType),DEvar (DVar 1 ShapeVar IntType)],DCall 2 [DEvar (DVar 0 ShapeVar IntType),DEvar (DVar 2 ShapeVar IntType)],DWrite (DStrConst "a1 = "),DWrite (DEvar (DVar 1 ShapeVar IntType)),DWrite (DStrConst ", a2 = "),DWrite (DEvar (DVar 2 ShapeVar IntType)),DWrite (DStrConst "\\n")] 3,DProc 1 [DPara 0 InVal IntType,DPara 1 InRef IntType] [DIf (DBinaryOp Op_lt (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 0) IntType) [DWrite (DStrConst "Incorrect input\\n")] [DIf (DBinaryOp Op_le (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 2) IntType) [DAssign (DVar 1 ShapeVar IntType) (DIntConst 1)] [DCall 1 [DBinaryOp Op_sub (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 1) IntType,DEvar (DVar 2 (ShapeArr 2) IntType)],DCall 1 [DBinaryOp Op_sub (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 1) IntType,DEvar (DVar 2 (ShapeArr 2) IntType)],DAssign (DVar 1 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 2 (ShapeArr 2) IntType)) (DEvar (DVar 2 (ShapeArr 2) IntType)) IntType)]]] 4,DProc 2 [DPara 0 InVal IntType,DPara 1 InRef IntType] [DIf (DBinaryOp Op_lt (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 0) IntType) [DWrite (DStrConst "Incorrect input\\n")] [DIf (DBinaryOp Op_le (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 2) IntType) [DAssign (DVar 1 ShapeVar IntType) (DIntConst 1)] [DAssign (DVar 2 ShapeVar IntType) (DIntConst 1),DAssign (DVar 3 ShapeVar IntType) (DIntConst 1),DAssign (DVar 5 ShapeVar IntType) (DIntConst 2),DWhile (DBinaryOp Op_lt (DEvar (DVar 5 ShapeVar IntType)) (DEvar (DVar 0 ShapeVar IntType)) IntType) [DAssign (DVar 4 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 2 ShapeVar IntType)) (DEvar (DVar 3 ShapeVar IntType)) IntType),DAssign (DVar 2 ShapeVar IntType) (DEvar (DVar 3 ShapeVar IntType)),DAssign (DVar 3 ShapeVar IntType) (DEvar (DVar 4 ShapeVar IntType)),DAssign (DVar 5 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 5 ShapeVar IntType)) (DIntConst 1) IntType)],DAssign (DVar 1 ShapeVar IntType) (DEvar (DVar 4 ShapeVar IntType))]]] 6])
      mapM_ putStrLn (map instructionFormatter ins)
