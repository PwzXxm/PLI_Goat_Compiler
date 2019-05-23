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
genProgram (DProgram mainId dProcs)
  = do 
      appendIns (ICall $ "proc_" ++ (show mainId))
      appendIns (IHalt)
      mapM_ genProc dProcs

genProc :: DProc -> Analyzer ()
genProc (DProc procId dParas dDecls dStmts slotSize)
  = do
      appendIns (IComment $ "Code for procedure " ++ (show procId))
      appendIns (ILabel $ "proc_" ++ (show procId))
      appendIns (IPushStack slotSize)




runCodeGenerator :: DGoatProgram -> [Instruction]
runCodeGenerator dGoatProgram
  = let state = Gstate { regCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []

test = runCodeGenerator (DProgram 0 [DProc 0 [] [DDecl 0 IntType ShapeVar,DDecl 1 IntType ShapeVar,DDecl 2 IntType ShapeVar] [DRead (DVar 0 ShapeVar IntType),DCall 1 [DEvar (DVar 0 ShapeVar IntType),DEvar (DVar 1 ShapeVar IntType)],DCall 2 [DEvar (DVar 0 ShapeVar IntType),DEvar (DVar 2 ShapeVar IntType)],DWrite (DStrConst "a1 = "),DWrite (DEvar (DVar 1 ShapeVar IntType)),DWrite (DStrConst ", a2 = "),DWrite (DEvar (DVar 2 ShapeVar IntType)),DWrite (DStrConst "\\n")] 3,DProc 1 [DPara 0 InVal IntType,DPara 1 InRef IntType] [DDecl 2 IntType (ShapeArr 2)] [DIf (DBinaryOp Op_lt (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 0) IntType) [DWrite (DStrConst "Incorrect input\\n")] [DIf (DBinaryOp Op_le (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 2) IntType) [DAssign (DVar 1 ShapeVar IntType) (DIntConst 1)] [DCall 1 [DBinaryOp Op_sub (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 1) IntType,DEvar (DVar 2 (ShapeArr 2) IntType)],DCall 1 [DBinaryOp Op_sub (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 1) IntType,DEvar (DVar 2 (ShapeArr 2) IntType)],DAssign (DVar 1 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 2 (ShapeArr 2) IntType)) (DEvar (DVar 2 (ShapeArr 2) IntType)) IntType)]]] 4,DProc 2 [DPara 0 InVal IntType,DPara 1 InRef IntType] [DDecl 2 IntType ShapeVar,DDecl 3 IntType ShapeVar,DDecl 4 IntType ShapeVar,DDecl 5 IntType ShapeVar] [DIf (DBinaryOp Op_lt (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 0) IntType) [DWrite (DStrConst "Incorrect input\\n")] [DIf (DBinaryOp Op_le (DEvar (DVar 0 ShapeVar IntType)) (DIntConst 2) IntType) [DAssign (DVar 1 ShapeVar IntType) (DIntConst 1)] [DAssign (DVar 2 ShapeVar IntType) (DIntConst 1),DAssign (DVar 3 ShapeVar IntType) (DIntConst 1),DAssign (DVar 5 ShapeVar IntType) (DIntConst 2),DWhile (DBinaryOp Op_lt (DEvar (DVar 5 ShapeVar IntType)) (DEvar (DVar 0 ShapeVar IntType)) IntType) [DAssign (DVar 4 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 2 ShapeVar IntType)) (DEvar (DVar 3 ShapeVar IntType)) IntType),DAssign (DVar 2 ShapeVar IntType) (DEvar (DVar 3 ShapeVar IntType)),DAssign (DVar 3 ShapeVar IntType) (DEvar (DVar 4 ShapeVar IntType)),DAssign (DVar 5 ShapeVar IntType) (DBinaryOp Op_add (DEvar (DVar 5 ShapeVar IntType)) (DIntConst 1) IntType)],DAssign (DVar 1 ShapeVar IntType) (DEvar (DVar 4 ShapeVar IntType))]]] 6])
