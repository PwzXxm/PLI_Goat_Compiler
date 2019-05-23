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
  , varibles :: M.Map String DVar
  , slotCounter :: Int
  , procCounter :: Int
  }

data SemanticError = SemanticError SourcePos String

instance Show SemanticError where
  show (SemanticError pos str) = (show pos) ++ " " ++ str

type Analyzer a = StateT Astate (Either SemanticError) a

getProcProto :: String -> SourcePos -> Analyzer DProcProto
getProcProto name sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then return $ (procs st) M.! name
        else lift $ throwError (SemanticError sourcePos ("Procedure named " ++ name ++ " does not exist"))

putProcProto :: String -> DProcProto -> SourcePos -> Analyzer ()
putProcProto name dProcProto sourcePos
  = do
      st <- get
      if M.member name (procs st)
        then lift $ throwError (SemanticError sourcePos ("Procedure named " ++ name ++ " already exists"))
        else put st {procs = M.insert name dProcProto (procs st)}

getVar :: String -> SourcePos -> Analyzer DVar
getVar name sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then return $ (varibles st) M.! name
        else lift $ throwError (SemanticError sourcePos ("Variable named " ++ name ++ " does not exist"))

putVar :: String -> DVar -> SourcePos -> Analyzer ()
putVar name var sourcePos
  = do
      st <- get
      if M.member name (varibles st)
        then lift $ throwError (SemanticError sourcePos ("Variable named " ++ name ++ " already exists"))
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
          let dParas = map (\(Para _ _ baseType indi) -> (DProcProtoPara indi baseType)) paras
          pid <- getProcCounter
          putProcProto ident (DProcProto pid dParas) sourcePos
          return ()
        ) procs

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
          putVar ident (DVar sc (ShapeVar) baseType) sourcePos
          return (DPara sc indi baseType)
        ) paras
      -- declarations
      dDecls <- mapM (\(Decl sourcePos ident baseType shape) -> 
        do
          sc <- getSlotCounter (getVarSizeByShape shape)
          putVar ident (DVar sc shape baseType) sourcePos
          return (DDecl sc baseType shape)
        ) decls
      -- the current counter + 1 is the total size
      totalSize <- getSlotCounter 1

      -- statements
      dStmts <- mapM checkStat stmts

      return (DProc pid dParas dDecls dStmts totalSize)

checkStat :: Stmt -> Analyzer DStmt
checkStat (Assign sourcePos (Var _ ident _) expr)
  = do
      dVar <- getVar ident sourcePos
      dExpr <- checkExpr expr
      return $ DAssign dVar dExpr
checkStat (Read sourcePos (Var _ ident _))
  = do
      dVar <- getVar ident sourcePos
      return $ DRead dVar
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
checkExpr (Evar sourcePos (Var _ ident _))
  = do
      dVar <- getVar ident sourcePos
      return $ DEvar dVar
checkExpr (BinaryOp sourcePos binop expr1 expr2)
  = do
      dExpr1 <- checkExpr expr1
      dExpr2 <- checkExpr expr2
      if cmpBaseType dExpr1 dExpr2
        then return $ DBinaryOp binop dExpr1 dExpr2 (getBaseType dExpr1)
        else lift $ throwError (SemanticError sourcePos ("different type varibles"))
checkExpr (UnaryMinus _ expr)
  = do
      dExpr <- checkExpr expr
      return $ DUnaryMinus dExpr (getBaseType dExpr)
checkExpr (UnaryNot _ expr)
  = do
      dExpr <- checkExpr expr
      return $ DUnaryNot dExpr (getBaseType dExpr)

-- test :: Analyzer DPara
-- test
--   = do
--     s <- getSlotCounter 1
--     return (DPara s InVal BoolType)

getBaseType :: DExpr -> BaseType
getBaseType (DBoolConst bool) = BoolType
getBaseType (DIntConst int) = IntType
getBaseType (DFloatConst float) = FloatType
-- getBaseType (StrConst string) = DStrConst string
getBaseType (DEvar (DVar _ _ baseType)) = baseType
getBaseType (DBinaryOp _ _ _ baseType) = baseType
getBaseType (DUnaryMinus _ baseType) = baseType
getBaseType (DUnaryNot _ baseType) = baseType

cmpBaseType :: DExpr -> DExpr -> Bool
cmpBaseType e1 e2 = (getBaseType e1) == (getBaseType e2)

getVarSizeByShape :: Shape -> Int
getVarSizeByShape (ShapeVar) = 1
getVarSizeByShape (ShapeArr a) = a
getVarSizeByShape (ShapeMat a b) = a * b
