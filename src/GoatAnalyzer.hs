-- | Semantic analyzer: check and add info to Decorated AST
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatAnalyzer where

import GoatAST
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Text.Parsec.Pos

data Astate = Astate
  { procs :: M.Map String DProcProto
  , varibles :: M.Map String DVarInfo
  , slotCounter :: Int
  , procCounter :: Int
  }

type Analyzer a = StateT Astate (Either SemanticError) a

-----------------------------------

data SemanticError = SemanticError SourcePos String

instance Show SemanticError where
  show (SemanticError pos str) = "Semantic error at " ++ (show pos) ++ ": " ++ str

throwSemanticErr :: SourcePos -> String -> Analyzer a
throwSemanticErr sourcePos msg = liftEither $ throwError (SemanticError sourcePos msg)

-----------------------------------

getProcProto :: String -> SourcePos -> Analyzer DProcProto
getProcProto name sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then return $ (procs st) M.! name
        else throwSemanticErr sourcePos ("Procedure named " ++ name ++ " does not exist")

putProcProto :: String -> DProcProto -> SourcePos -> Analyzer ()
putProcProto name dProcProto sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then throwSemanticErr sourcePos ("Procedure named " ++ name ++ " already exists")
        else put st {procs = M.insert name dProcProto (procs st)}

getVarInfo :: String -> SourcePos -> Analyzer DVarInfo
getVarInfo name sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then return $ (varibles st) M.! name
        else throwSemanticErr sourcePos ("Variable named " ++ name ++ " does not exist")

putVar :: String -> DVarInfo -> SourcePos -> Analyzer ()
putVar name var sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then throwSemanticErr sourcePos ("Variable named " ++ name ++ " already exists")
        else put st{varibles = M.insert name var (varibles st)}

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

resetProcCounter :: Analyzer ()
resetProcCounter
  = do
      st <- get
      put st{procCounter = 0}

-----------------------------------

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

semanticCheckDGoatProgram :: GoatProgram -> Analyzer DGoatProgram
semanticCheckDGoatProgram (Program procs)
  = do
      loadProcProto procs
      dProcs <- mapM checkProc procs
      (DProcProto mainId _) <- getProcProto "main" (newPos "" 0 0)
      return $ DProgram mainId dProcs



-- load all procedure's prototype and check for duplicate identity
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


checkProc :: Proc -> Analyzer DProc
checkProc (Proc sourcePos ident paras decls stmts)
  = do
      resetVar
      resetSlotCounter
      (DProcProto pid _) <- getProcProto ident sourcePos
      -- parameters
      dParas <- mapM (\(Para sourcePos ident baseType indi) -> 
        do
          sc <- getSlotCounter 1
          putVar ident (DVarInfo sc (ShapeVar) (convType baseType)) sourcePos
          return (DPara sc indi (convType baseType))
        ) paras
      -- declarations
      mapM_ (\(Decl sourcePos ident baseType shape) -> 
        do
          sc <- getSlotCounter (getVarSizeByShape shape)
          putVar ident (DVarInfo sc shape (convType baseType)) sourcePos
          return ()
        ) decls
      -- the current counter + 1 is the total size
      totalSize <- getSlotCounter 1

      -- statements
      dStmts <- mapM checkStat stmts

      return (DProc pid dParas dStmts totalSize)

checkStat :: Stmt -> Analyzer DStmt
checkStat (Assign _ (Var sourcePos ident idx) expr)
  = do
    -- TODO: check type
      (DVarInfo slotNum shape dBaseType) <- getVarInfo ident sourcePos
      dExpr <- checkExpr expr
      dIdx <- checkIdx idx
      return $ DAssign (DVar slotNum dIdx dBaseType) dExpr
checkStat (Read _ (Var sourcePos ident idx))
  = do
    -- TODO: check type
      (DVarInfo slotNum shape dBaseType) <- getVarInfo ident sourcePos
      dIdx <- checkIdx idx
      return $ DRead (DVar slotNum dIdx dBaseType)
checkStat (Write sourcePos expr)
  = do
      dExpr <- checkExpr expr
      return $ DWrite dExpr
checkStat (Call sourcePos ident exprs)
  = do
      dExprs <- mapM checkExpr exprs
      (DProcProto procId _) <- getProcProto ident sourcePos
      return $ DCall procId dExprs
checkStat (If sourcePos expr stmts1 stmts2)
  = do
      dExpr <- checkExpr expr
      dStmts1 <- mapM checkStat stmts1
      dStmts2 <- mapM checkStat stmts2
      return $ DIf dExpr dStmts1 dStmts2
checkStat (While sourcePos expr stmts)
  = do
      dExpr <- checkExpr expr
      dStmts <- mapM checkStat stmts
      return $ DWhile dExpr dStmts

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
      dIdx <- checkIdx idx
      -- check shape and idx
      return $ DEvar (DVar slotNum dIdx dBaseType)
checkExpr (BinaryOp sourcePos binop expr1 expr2)
  = do
      dExpr1 <- checkExpr expr1
      dExpr2 <- checkExpr expr2
      dBaseType <- checkBaseType dExpr1 dExpr2 binop sourcePos
      return $ DBinaryOp binop dExpr1 dExpr2 dBaseType
checkExpr (UnaryMinus _ expr)
  = do
      dExpr <- checkExpr expr
      return $ DUnaryMinus dExpr (getBaseType dExpr)
checkExpr (UnaryNot _ expr)
  = do
      dExpr <- checkExpr expr
      return $ DUnaryNot dExpr (getBaseType dExpr)

getBaseType :: DExpr -> DBaseType
getBaseType (DBoolConst bool) = DBoolType
getBaseType (DIntConst int) = DIntType
getBaseType (DFloatConst float) = DFloatType
getBaseType (DStrConst string) = DStringType
getBaseType (DEvar (DVar _ _ dBaseType)) = dBaseType
getBaseType (DBinaryOp _ _ _ dBaseType) = dBaseType
getBaseType (DUnaryMinus _ dBaseType) = dBaseType
getBaseType (DUnaryNot _ dBaseType) = dBaseType

checkBaseType :: DExpr -> DExpr -> Binop -> SourcePos -> Analyzer DBaseType
checkBaseType e1 e2 binop sourcePos
  | binop == Op_add || binop == Op_sub || binop == Op_mul || binop == Op_div
  = do
      if (getBaseType e1) == DBoolType || (getBaseType e2) == DBoolType
        then throwSemanticErr sourcePos ("binary arithmetic operator must have numeric type")
        else
          if (getBaseType e1) == DFloatType || (getBaseType e2) == DFloatType
            then return DFloatType
            else return DIntType
-- checkBaseType e1 e2 binop sourcePos
--   | binop == 

-- check index int
checkIdx :: Idx -> Analyzer DIdx
checkIdx IdxVar
  = do
      return DIdxVar
checkIdx (IdxArr expr)
  = do
      dExpr <- checkExpr expr
      return $ DIdxArr dExpr
checkIdx (IdxMat expr1 expr2)
  = do
      dExpr1 <- checkExpr expr1
      dExpr2 <- checkExpr expr2
      return $ DIdxMat dExpr1 dExpr2


getVarSizeByShape :: Shape -> Int
getVarSizeByShape (ShapeVar) = 1
getVarSizeByShape (ShapeArr a) = a
getVarSizeByShape (ShapeMat a b) = a * b
