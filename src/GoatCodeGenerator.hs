module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid
import OzInstruction
import GoatAST
import GoatAnalyzer


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

      -- statement
      mapM_ genStmt dStmts

      appendIns (IPopStack slotSize)
      appendIns (IReturn)

genStmt :: DStmt -> Generator ()
genStmt (DAssign dVar dExpr)
  = do
      reg0 <- getReg
      evalExpr reg0 dExpr
      saveToVar reg0 dVar
      setNextUnusedReg reg0

genStmt (DWrite dExpr)
  = do
      let dBaseType = getBaseType dExpr
      reg0 <- getReg
      evalExpr reg0 dExpr
      let cmd = case dBaseType of DStringType -> "print_string"
                                  DBoolType   -> "print_bool"
                                  DIntType    -> "print_int"
                                  DFloatType  -> "print_real"
      appendIns (ICall_bt cmd)
      setNextUnusedReg reg0

genStmt (DRead dVar)
  = do
      let (DVar _ _ dBaseType) = dVar
      let cmd = case dBaseType of DBoolType  -> "read_bool"
                                  DIntType   -> "read_int"
                                  DFloatType -> "read_real"
      appendIns (ICall_bt cmd)
      reg0 <- getReg
      saveToVar reg0 dVar
      setNextUnusedReg reg0


genStmt _
  = do
      return ()

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

saveToVar ::Int -> DVar -> Generator ()
saveToVar tReg (DVar slotNum (DIdxVar) dBaseType)
  = appendIns (IStatement $ Store slotNum tReg )

evalExpr :: Int -> DExpr -> Generator ()
evalExpr tReg (DBoolConst v)
  = appendIns (IConstant $ ConsInt tReg $ boolToInt v)

evalExpr tReg (DIntConst v)
  = appendIns (IConstant $ ConsInt tReg v)

evalExpr tReg (DFloatConst v)
  = appendIns (IConstant $ ConsFloat tReg v)

evalExpr tReg (DStrConst v)
  = appendIns (IConstant $ ConsString tReg v)

runCodeGenerator :: DGoatProgram -> [Instruction]
runCodeGenerator dGoatProgram
  = let state = Gstate { regCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []

test 
  = do 
      let ins = runCodeGenerator (DProgram 0 [DProc 0 [] [DWrite (DStrConst "asd\\nsadfasdfa"),DWrite (DStrConst "Test\\n")] 0])
      mapM_ putStrLn (map instructionFormatter ins)
