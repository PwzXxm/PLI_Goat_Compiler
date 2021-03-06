-- | Semantic analyzer: check and convert an AST info to Decorated AST
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatAnalyzer(runSemanticCheck) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M
import           GoatAST
import           Text.Parsec.Pos

-- | State to store information of each procedure
data Astate = Astate
  { procs       :: M.Map String DProcProto
  , varibles    :: M.Map String DVarInfo
  , slotCounter :: Int
  , procCounter :: Int
  }

type Analyzer a = StateT Astate (Either SemanticError) a

-----------------------------------
-- | Print Semantics Error

data SemanticError = SemanticError SourcePos String

instance Show SemanticError where
  show (SemanticError pos str) = "Semantic error at " ++ (show pos) ++ ": " ++ str

throwSemanticErr :: SourcePos -> String -> Analyzer a
throwSemanticErr sourcePos msg = liftEither $ throwError (SemanticError sourcePos msg)

-----------------------------------

-- | Check if the procedure exists
getProcProto :: String -> SourcePos -> Analyzer DProcProto
getProcProto name sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then return $ (procs st) M.! name
        else throwSemanticErr sourcePos ("Procedure named " ++ name ++ " does not exist")

-- | Update the procedure
putProcProto :: String -> DProcProto -> SourcePos -> Analyzer ()
putProcProto name dProcProto sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then throwSemanticErr sourcePos ("Procedure named " ++ name ++ " already exists")
        else put st {procs = M.insert name dProcProto (procs st)}

-- | Check if the varible exists
getVarInfo :: String -> SourcePos -> Analyzer DVarInfo
getVarInfo name sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then return $ (varibles st) M.! name
        else throwSemanticErr sourcePos ("Variable named " ++ name ++ " does not exist")

-- | Update the varible
putVar :: String -> DVarInfo -> SourcePos -> Analyzer ()
putVar name var sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then throwSemanticErr sourcePos ("Variable named " ++ name ++ " already exists")
        else put st{varibles = M.insert name var (varibles st)}

-- | Clean up varibles in the state
resetVar :: Analyzer ()
resetVar
  = do
      st <- get
      put st{varibles = M.empty}

getSlotCounter :: Int -> Analyzer Int
getSlotCounter s
  = do
      st <- get
      put st{slotCounter = (slotCounter st) + s}
      return $ slotCounter st

-- | Clean up slot counter in the state
resetSlotCounter :: Analyzer ()
resetSlotCounter
  = do
      st <- get
      put st{slotCounter = 0}

getProcCounter :: Analyzer Int
getProcCounter
  = do
      st <- get
      put st{procCounter = (procCounter st) + 1}
      return $ procCounter st

-- | Clean up procedure counter in the state
resetProcCounter :: Analyzer ()
resetProcCounter
  = do
      st <- get
      put st{procCounter = 0}

-----------------------------------

-- | Main funtion of doing semantic check on a Goat program
runSemanticCheck :: GoatProgram -> Either SemanticError DGoatProgram
runSemanticCheck tree
  = do
      let
        state = Astate
          { procs = M.empty
          , varibles = M.empty
          , slotCounter = 0
          , procCounter = 0
          }
      r <- evalStateT (semanticCheckDGoatProgram tree) state
      return r


-----------------------------------

-- | Apply semantic checking on a Goat program
semanticCheckDGoatProgram :: GoatProgram -> Analyzer DGoatProgram
semanticCheckDGoatProgram (Program procs)
  = do
      loadProcProto procs
      (DProcProto mainId _) <- getProcProto "main" (newPos "" 0 0)
      dProcs <- mapM checkProc procs
      return $ DProgram mainId dProcs


-- | Load all procedure's prototype and check for duplicate identity
loadProcProto :: [Proc] -> Analyzer ()
loadProcProto procs
    = mapM_ (\(Proc sourcePos ident paras _ _) ->
        do
          -- do not check the parameter for now
          -- will be checked when analysing the procedure
          let dParas = map (\(Para _ _ baseType indi) -> (DProcProtoPara indi (convType baseType))) paras
          pid <- getProcCounter
          putProcProto ident (DProcProto pid dParas) sourcePos
          return ()
        ) procs


convType :: BaseType -> DBaseType
convType BoolType  = DBoolType
convType IntType   = DIntType
convType FloatType = DFloatType


-- | Apply semantic checking on one procedure
checkProc :: Proc -> Analyzer DProc
checkProc (Proc sourcePos ident paras decls stmts)
  = do
      resetVar
      resetSlotCounter
      (DProcProto pid _) <- getProcProto ident sourcePos
      -- parameters
      if ident == "main" && length paras > 0
        then throwSemanticErr sourcePos ("Main procedure can not have parameter")
        else do
          mapM_ (\(Para sourcePos ident baseType indi) ->
            do
              sc <- getSlotCounter 1
              putVar ident (DVarInfo sc (getDShape ShapeVar indi) (convType baseType)) sourcePos
            ) paras
          -- declarations
          dVarInfos <- mapM (\(Decl sourcePos ident baseType shape) ->
            do
              sc <- getSlotCounter (getVarSizeByShape shape)
              let dVarInfo = (DVarInfo sc (getDShape shape InVal) (convType baseType))
              putVar ident dVarInfo sourcePos
              return dVarInfo
            ) decls
          -- the current counter + 1 is the total size
          totalSize <- getSlotCounter 1

          -- statements
          dStmts <- mapM checkStat stmts

          return (DProc pid (length paras) dStmts dVarInfos totalSize)


-- | Applyt semantic checking on one statement
checkStat :: Stmt -> Analyzer DStmt
checkStat (Assign _ (Var sourcePos ident idx) expr)
  = do
      (DVarInfo slotNum shape dBaseType) <- getVarInfo ident sourcePos
      dExpr <- checkExpr expr
      dIdx <- checkShapeAndIdx shape idx sourcePos
      if dBaseType == (getBaseType dExpr)
        then return $ DAssign sourcePos (DVar slotNum dIdx dBaseType) dExpr
        else
          -- allow Float variable to be assigned by Int expression/const
          if dBaseType == DFloatType && (getBaseType dExpr) == DIntType
            then return $ DAssign sourcePos (DVar slotNum dIdx dBaseType) (DIntToFloat dExpr)
            else throwSemanticErr sourcePos ("Two sides of the expression have different types")

checkStat (Read _ (Var sourcePos ident idx))
  = do
      (DVarInfo slotNum shape dBaseType) <- getVarInfo ident sourcePos
      dIdx <- checkShapeAndIdx shape idx sourcePos
      return $ DRead sourcePos (DVar slotNum dIdx dBaseType)

checkStat (Write sourcePos expr)
  = do
      dExpr <- checkExpr expr
      return $ DWrite sourcePos dExpr

checkStat (Call sourcePos ident exprs)
  = do
      (DProcProto procId paras) <- getProcProto ident sourcePos
      -- check the number of arguments in a procedure call matches
      -- with the number of parameters in a procdefure defination
      if (length exprs) /= (length paras)
        then throwSemanticErr sourcePos ("The function " ++ ident ++ " need " ++ (show (length paras)) ++ " parameter, but input have " ++ (show (length exprs)))
        else do
          -- check if types are matched
          dCallParas <- mapM checkProcAndExpr (zip paras exprs)
          return $ DCall sourcePos procId dCallParas

checkStat (If sourcePos expr stmts1 stmts2)
  = do
      dExpr <- checkExpr expr
      if (getBaseType dExpr) /= DBoolType
        then throwSemanticErr sourcePos ("A boolean constant/expression is needed as a condition")
        else do
          dStmts1 <- mapM checkStat stmts1
          dStmts2 <- mapM checkStat stmts2
          return $ DIf sourcePos dExpr dStmts1 dStmts2

checkStat (While sourcePos expr stmts)
  = do
      dExpr <- checkExpr expr
      if (getBaseType dExpr) /= DBoolType
        then throwSemanticErr sourcePos ("A boolean constant/expression is needed as a condition")
        else do
          dStmts <- mapM checkStat stmts
          return $ DWhile sourcePos dExpr dStmts


-- | Applyt semantic checking on one expression
checkExpr :: Expr -> Analyzer DExpr
checkExpr (BoolConst _ bool)
  = do
      return $ DBoolConst bool
checkExpr (IntConst _ int)
  = do
      return $ DIntConst int
checkExpr (FloatConst _ float)
  = do
      return $ DFloatConst float
checkExpr (StrConst _ string)
  = do
      return $ DStrConst string
checkExpr (Evar sourcePos (Var _ ident idx))
  = do
      (DVarInfo slotNum shape dBaseType) <- getVarInfo ident sourcePos
      dIdx <- checkShapeAndIdx shape idx sourcePos
      return $ DEvar (DVar slotNum dIdx dBaseType)
checkExpr (BinaryOp sourcePos binop expr1 expr2)
  = do
      dExpr1 <- checkExpr expr1
      dExpr2 <- checkExpr expr2
      (dBaseType, dExpr1, dExpr2) <- checkBaseType dExpr1 dExpr2 binop sourcePos
      return $ DBinaryOp binop dExpr1 dExpr2 dBaseType
checkExpr (UnaryMinus sourcePos expr)
  = do
      dExpr <- checkExpr expr
      if (getBaseType dExpr) == DIntType || (getBaseType dExpr) == DFloatType
        then return $ DUnaryMinus dExpr (getBaseType dExpr)
        else throwSemanticErr sourcePos ("The operand of unary minus must be of type int or float")
checkExpr (UnaryNot sourcePos expr)
  = do
      dExpr <- checkExpr expr
      if (getBaseType dExpr) == DBoolType
        then return $ DUnaryNot dExpr (getBaseType dExpr)
        else throwSemanticErr sourcePos ("The operand of unary not must be of type boolean")


-- | Checking correctness of type of the two operands of a binary operation
checkBaseType :: DExpr -> DExpr -> Binop -> SourcePos -> Analyzer (DBaseType,DExpr,DExpr)
checkBaseType e1 e2 binop sourcePos
  | binop == Op_add || binop == Op_sub || binop == Op_mul || binop == Op_div
    = do
        if (getBaseType e1) == DBoolType || (getBaseType e2) == DBoolType || (getBaseType e1) == DStringType || (getBaseType e2) == DStringType
          then throwSemanticErr sourcePos ("The two operands of a binary arithmetic operator must have numeric type")
          else
            if (getBaseType e1) == (getBaseType e2)
              then return ((getBaseType e1), e1, e2)
              else
                if (getBaseType e1) == DFloatType
                  then return (DFloatType, e1, (DIntToFloat e2))
                  else return (DFloatType, (DIntToFloat e1), e2)
  | binop == Op_eq || binop == Op_ne
    = do
        if (getBaseType e1) == DStringType || (getBaseType e2) == DStringType
          then throwSemanticErr sourcePos ("The two operands of operator cannot be String type")
          else
            if (getBaseType e1) == (getBaseType e2)
              then return (DBoolType, e1, e2)
              else throwSemanticErr sourcePos ("The two operands of = and != must be same type")
  | binop == Op_lt || binop == Op_le || binop == Op_gt || binop == Op_ge
    = do
        if (getBaseType e1) == DStringType || (getBaseType e2) == DStringType
          then throwSemanticErr sourcePos ("The two operands of operator cannot be String type")
          else
            if (getBaseType e1) == (getBaseType e2)
              then return (DBoolType, e1, e2)
              else
                if (getBaseType e1) == DBoolType || (getBaseType e2) == DBoolType
                  then throwSemanticErr sourcePos ("Cannot campare a Boolean type and a numeric type")
                  else
                    if (getBaseType e1) == DFloatType
                      then return (DBoolType, e1, (DIntToFloat e2))
                      else return (DBoolType, (DIntToFloat e1), e2)
  | otherwise -- Op_and, Op_or
    = do
        if (getBaseType e1) == DStringType || (getBaseType e2) == DStringType
          then throwSemanticErr sourcePos ("The two operands of operator cannot be String type")
          else
            if (getBaseType e1) == DBoolType && (getBaseType e2) == DBoolType
              then return (DBoolType, e1, e2)
              else throwSemanticErr sourcePos ("The two operands of && and || must be Boolean type")


-- | Check the consistency of shape and index of array and matrix
checkShapeAndIdx :: DShape -> Idx -> SourcePos -> Analyzer DIdx
checkShapeAndIdx (DShapeVar isAddress) IdxVar _
  = do
      return (DIdxVar isAddress)
checkShapeAndIdx (DShapeArr _) (IdxArr expr) sourcePos
  = do
      dExpr <- checkExpr expr
      if (getBaseType dExpr) == DIntType
        then return $ DIdxArr dExpr
        else throwSemanticErr sourcePos ("Index should be Int not " ++ (show (getBaseType dExpr)))
checkShapeAndIdx (DShapeMat _ sec) (IdxMat expr1 expr2) sourcePos
  = do
      dExpr1 <- checkExpr expr1
      dExpr2 <- checkExpr expr2
      if (getBaseType dExpr1) == DIntType && (getBaseType dExpr2) == DIntType
        then return $ DIdxMat dExpr1 dExpr2 sec
        else throwSemanticErr sourcePos ("Index should be Int not " ++ (show (getBaseType dExpr1)) ++ " and " ++ (show (getBaseType dExpr2)))
checkShapeAndIdx _ _ sourcePos
  = do
      throwSemanticErr sourcePos ("Index type and shape type are different")


-- | Check if the parameter is correctively passed with value or reference
checkProcAndExpr :: (DProcProtoPara,Expr) -> Analyzer DCallPara
checkProcAndExpr ((DProcProtoPara InRef dBaseType1), expr)
  = case expr of
      (Evar sourcePos _) -> do
        dExpr <- checkExpr expr
        let (DEvar (DVar slotNum dIdx dBaseType2)) = dExpr

        if dBaseType1 == dBaseType2
          then return $ DCallParaRef (DVar slotNum dIdx dBaseType2)
          else throwSemanticErr sourcePos ("Types are not match, expected type: " ++ (show dBaseType1) ++ "\nactual type: " ++ (show dBaseType2))
      _ ->
        throwSemanticErr (getExprSourcePos expr) ("Expected Reference varible here")
checkProcAndExpr ((DProcProtoPara InVal dBaseType), expr)
  = do
      dExpr <- checkExpr expr
      if dBaseType == (getBaseType dExpr)
        then return $ DCallParaVal dExpr
        else
          if dBaseType == DFloatType && (getBaseType dExpr) == DIntType
            then return $ DCallParaVal (DIntToFloat dExpr)
            else throwSemanticErr (getExprSourcePos expr) ("Types are not match")


getDShape :: Shape -> Indi -> DShape
getDShape ShapeVar InVal     = DShapeVar False
getDShape ShapeVar InRef     = DShapeVar True
getDShape (ShapeArr i) _     = DShapeArr i
getDShape (ShapeMat i1 i2) _ = DShapeMat i1 i2


getVarSizeByShape :: Shape -> Int
getVarSizeByShape (ShapeVar)     = 1
getVarSizeByShape (ShapeArr a)   = a
getVarSizeByShape (ShapeMat a b) = a * b
