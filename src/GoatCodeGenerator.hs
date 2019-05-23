module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid
import OzInstruction
import GoatAST


data Gstate = Gstate 
  { regCounter :: Int, labelCounter :: Int, instructions :: Endo [Instruction]}

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

getLabel :: String -> Generator String
getLabel prefix
  = do
      st <- get
      put st{labelCounter = (labelCounter st) + 1}
      return $ prefix ++ show (labelCounter st)
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

      appendIns (IComment $ "end of procedure")
      appendIns (IPopStack slotSize)
      appendIns (IReturn)

genStmt :: DStmt -> Generator ()
genStmt (DAssign dVar dExpr)
  = do
      appendIns (IComment $ "stmt: assignment")
      reg0 <- getReg
      evalExpr reg0 dExpr
      saveToVar reg0 dVar
      setNextUnusedReg reg0

genStmt (DWrite dExpr)
  = do
      appendIns (IComment $ "stmt: write")
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
      appendIns (IComment $ "stmt: read")
      let (DVar _ _ dBaseType) = dVar
      let cmd = case dBaseType of DBoolType  -> "read_bool"
                                  DIntType   -> "read_int"
                                  DFloatType -> "read_real"
      appendIns (ICall_bt cmd)
      reg0 <- getReg
      saveToVar reg0 dVar
      setNextUnusedReg reg0

-- genStmt (DCall procId dExprs)
--   = do
--       appendIns (IComment $ "stmt: call" ++ (show procId))


genStmt (DIf dExpr dStmts dEStmts)
  = do
      appendIns (IComment $ "stmt: if_condition")
      reg0 <- getReg
      label_then <- getLabel "if_"
      label_else <- getLabel "if_"
      label_end  <- getLabel "if_"

      evalExpr reg0 dExpr
      appendIns (IBranch $ Cond True reg0 label_then)
      appendIns (IBranch $ Uncond label_else)
      setNextUnusedReg reg0

      -- then case
      appendIns (ILabel $ label_then)
      appendIns (IComment $ "stmt: if_then")
      mapM_ genStmt dStmts
      appendIns (IBranch $ Uncond label_end)

      -- else case
      appendIns (ILabel $ label_else)
      appendIns (IComment $ "stmt: if_else")
      mapM_ genStmt dEStmts
      appendIns (IBranch $ Uncond label_end)

      appendIns (ILabel $ label_end)
      appendIns (IComment $ "stmt: if_end")

genStmt (DWhile dExpr dStmts)
  = do
      label_cond <- getLabel "while_"
      label_end  <- getLabel "while_"

      appendIns (ILabel $ label_cond)
      appendIns (IComment $ "stmt: while")

      reg0 <- getReg
      evalExpr reg0 dExpr
      appendIns (IBranch $ Cond False reg0 label_end)
      setNextUnusedReg reg0

      appendIns (IComment $ "stmt: while_body")
      mapM_ genStmt dStmts
      appendIns (IBranch $ Uncond label_cond)

      appendIns (ILabel $ label_end)
      appendIns (IComment $ "stmt: while_end")

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

saveToVar :: Int -> DVar -> Generator ()
saveToVar tReg (DVar slotNum (DIdxVar False) _)
  = appendIns (IStatement $ Store slotNum tReg)

saveToVar tReg dVar
  = do 
      addrReg <- getReg
      getVarAddress addrReg dVar
      appendIns (IStatement $ Store_in addrReg tReg)
      setNextUnusedReg addrReg


getVarAddress :: Int -> DVar -> Generator ()
getVarAddress tReg (DVar slotNum (DIdxVar isAddress) _)
  = do if isAddress
          then appendIns (IStatement $ Load tReg slotNum)
          else appendIns (IStatement $ Load_ad tReg slotNum)
-- 1d
getVarAddress tReg (DVar slotNum (DIdxArr dExpr) _)
  = do
      appendIns (IStatement $ Load_ad tReg slotNum)
      offsetReg <- getReg
      evalExpr offsetReg dExpr
      appendIns (IOperation $ Sub_off tReg tReg offsetReg)
      setNextUnusedReg offsetReg
-- 2d
getVarAddress tReg (DVar slotNum (DIdxMat dExpr1 dExpr2 secDimSize) _)
  = do
      appendIns (IStatement $ Load_ad tReg slotNum)
      -- dExpr1 * (secDimSize-1) + dExpr2
      offsetReg <- getReg
      evalExpr offsetReg dExpr1
      tmpReg <- getReg
      -- *
      appendIns (IConstant $ ConsInt tmpReg (secDimSize-1))
      appendIns (IOperation $ Binary MUL INT offsetReg offsetReg tmpReg)
      -- +
      evalExpr tmpReg dExpr2
      appendIns (IOperation $ Binary ADD INT offsetReg offsetReg tmpReg)
      -- 
      appendIns (IOperation $ Sub_off tReg tReg offsetReg)
      setNextUnusedReg offsetReg


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
  = let state = Gstate { regCounter = 0, labelCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []

test 
  = do 
      let ins = runCodeGenerator (DProgram 0 [DProc 0 [] [DIf (DBoolConst True) [DAssign (DVar 0 (DIdxVar False) DIntType) (DIntConst 1)] [DAssign (DVar 1 (DIdxVar False) DIntType) (DIntConst 1)]] 2])
      mapM_ putStrLn (map instructionFormatter ins)
