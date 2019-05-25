module GoatCodeGenerator where

import Control.Monad.State
import           Data.Monoid
import OzInstruction
import GoatAST
import Text.ParserCombinators.Parsec.Pos


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

getVarSizeByDShape :: DShape -> Int
getVarSizeByDShape (DShapeVar _) = 1
getVarSizeByDShape (DShapeArr a) = a
getVarSizeByDShape (DShapeMat a b) = a * b

genProc :: DProc -> Generator ()
genProc (DProc procId numOfParas dStmts dVarInfos slotSize)
  = do
      appendIns (ILabel $ "proc_" ++ (show procId))
      appendIns (IComment $ "code for procedure " ++ (show procId))
      appendIns (IPushStack slotSize)

      appendIns (IComment $ "load parameter")
      let paraSlotSize = numOfParas
      mapM_ (\i -> do appendIns (IStatement $ Store i i)) [0..(paraSlotSize-1)]

      appendIns (IComment $ "init variable")
      reg_int_0 <- getReg
      reg_float_0 <- getReg
      appendIns (IConstant $ ConsInt reg_int_0 0)
      appendIns (IConstant $ ConsFloat reg_float_0 0.0)
      mapM_ (\(DVarInfo slotNum dShape dBaseType) -> 
              do 
                let reg_init = if dBaseType == DFloatType then reg_float_0 else reg_int_0
                let endSlotNum = ((getVarSizeByDShape dShape) - 1)
                mapM_ (\i -> appendIns (IStatement $ Store i reg_init)) [slotNum..endSlotNum]
              ) dVarInfos
      setNextUnusedReg reg_int_0

      appendIns (IComment $ "procedure begin")
      -- statement
      mapM_ genStmt dStmts

      appendIns (IComment $ "procedure end")
      appendIns (IPopStack slotSize)
      appendIns (IReturn)

sourcePosComment :: SourcePos -> Generator ()
sourcePosComment sp
  = do
    appendIns (IComment $ "line: " ++ (show $ sourceLine sp) ++ ", column: " ++ (show $ sourceLine sp) )


genStmt :: DStmt -> Generator ()
genStmt (DAssign sourcePos dVar dExpr)
  = do
      sourcePosComment sourcePos
      appendIns (IComment $ "stmt: assignment")
      reg0 <- getReg
      evalExpr reg0 dExpr
      saveToVar reg0 dVar
      setNextUnusedReg reg0

genStmt (DWrite sourcePos dExpr)
  = do
      sourcePosComment sourcePos
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

genStmt (DRead sourcePos dVar)
  = do
      sourcePosComment sourcePos
      appendIns (IComment $ "stmt: read")
      let (DVar _ _ dBaseType) = dVar
      let cmd = case dBaseType of DBoolType  -> "read_bool"
                                  DIntType   -> "read_int"
                                  DFloatType -> "read_real"
      appendIns (ICall_bt cmd)
      reg0 <- getReg
      saveToVar reg0 dVar
      setNextUnusedReg reg0

genStmt (DCall sourcePos procId dCallParas)
  = do
      sourcePosComment sourcePos
      appendIns (IComment $ "stmt: call" ++ (show procId))
      mapM_ (
        \x -> do
          -- get registers from 0 to (length dCallParas) - 1
          reg <- getReg
          case x of 
            DCallParaVal dExpr -> evalExpr reg dExpr
            DCallParaRef dVar  -> getVarAddress reg dVar
          return ()) dCallParas

      appendIns (ICall $ "proc_" ++ (show procId))
      -- set back to 0
      setNextUnusedReg 0

genStmt (DIf sourcePos dExpr dStmts dEStmts)
  = do
      sourcePosComment sourcePos
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

genStmt (DWhile sourcePos dExpr dStmts)
  = do
      sourcePosComment sourcePos
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
      appendIns (IOperation $ Binary Mul INT offsetReg offsetReg tmpReg)
      -- +
      evalExpr tmpReg dExpr2
      appendIns (IOperation $ Binary Add INT offsetReg offsetReg tmpReg)
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

evalExpr tReg (DIntToFloat e0)
  = do
      evalExpr tReg e0
      appendIns (IOperation $ Int2real tReg tReg)

evalExpr tReg (DEvar dVar)
  = loadFromVar tReg dVar

evalExpr tReg (DBinaryOp binop e0 e1 dBaseType)
  = do
      r0 <- getReg
      evalExpr tReg e0
      evalExpr r0 e1
      genBinop binop tReg r0 (getBaseType e0)
      setNextUnusedReg r0

evalExpr tReg (DUnaryMinus e0 DFloatType)
  = do
      evalExpr tReg e0
      appendIns (IOperation $ Unary NEG REAL tReg tReg)

evalExpr tReg (DUnaryMinus e0 DIntType)
  = do
      evalExpr tReg e0
      appendIns (IOperation $ Unary NEG INT tReg tReg)

evalExpr tReg (DUnaryNot e0 _)
  = do
      evalExpr tReg e0
      appendIns (IOperation $ Not_ tReg tReg)

genIntToFloat :: Int -> DExpr -> Generator ()
genIntToFloat r e
  = do
      if (getBaseType e) == DIntType
        then appendIns (IOperation $ Int2real r r)
      else return ()

-- TODO: Binary op
genBinop :: Binop -> Int -> Int -> DBaseType -> Generator()
genBinop binop r0 r1 dBaseType
  | isLogicalBinop binop
    = do
        l0 <- getLabel "bool_op_"

        if binop == Op_and
          then do
            appendIns (IComment $ "logical operation AND")
            appendIns (IBranch $ Cond False r0 l0)
            appendIns (IOperation $ And_ r0 r0 r1)
          else do
            appendIns (IComment $ "logical operation OR")
            appendIns (IBranch $ Cond True r0 l0)
            appendIns (IOperation $ Or_ r0 r0 r1)

        appendIns (ILabel $ l0)

  | otherwise
    = if dBaseType == DFloatType
        then appendIns (IOperation $ Binary (getOzBinaryOp binop) REAL r0 r0 r1)
        else appendIns (IOperation $ Binary (getOzBinaryOp binop) INT r0 r0 r1)

getOzBinaryOp :: Binop -> BinaryOp
getOzBinaryOp Op_add = Add
getOzBinaryOp Op_sub = Sub
getOzBinaryOp Op_mul = Mul
getOzBinaryOp Op_div = Div
getOzBinaryOp Op_eq = Eq
getOzBinaryOp Op_ne = Ne
getOzBinaryOp Op_lt = Lt
getOzBinaryOp Op_le = Le
getOzBinaryOp Op_gt = Gt
getOzBinaryOp Op_ge = Ge

isLogicalBinop :: Binop -> Bool
isLogicalBinop Op_and = True
isLogicalBinop Op_or = True
isLogicalBinop _ = False

isRelationalBinop :: Binop -> Bool
isRelationalBinop Op_eq = True
isRelationalBinop Op_ne = True
isRelationalBinop Op_lt = True
isRelationalBinop Op_le = True
isRelationalBinop Op_gt = True
isRelationalBinop Op_ge = True
isRelationalBinop _ = False

-- TODO: load
loadFromVar :: Int -> DVar -> Generator()
loadFromVar tReg (DVar slotNum (DIdxVar False) _)
  = appendIns (IStatement $ Load tReg slotNum)

loadFromVar tReg dVar
  = do
      r0 <- getReg
      getVarAddress r0 dVar
      appendIns (IStatement $ Load_in tReg r0)
      setNextUnusedReg r0

runCodeGenerator :: DGoatProgram -> [Instruction]
runCodeGenerator dGoatProgram
  = let state = Gstate { regCounter = 0, labelCounter = 0, instructions = Endo ([]<>) }
        s = execState (genProgram dGoatProgram) state
    in (appEndo (instructions s)) []
