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

getProcProto :: String -> Analyzer DProcProto
getProcProto name
  = do
      st <- get
      return $ (procs st) M.! name

putProcProto :: String -> DProcProto -> Analyzer ()
putProcProto name dProcProto
  = do
      st <- get
      if M.member name (procs st)
        then lift $ throwError (SemanticError (newPos "" 0 0) ("Variable named " ++ name ++ " already exist"))
        else put st {procs = M.insert name dProcProto (procs st)}

getVar :: String -> Analyzer DVar
getVar var
  = do
      st <- get
      return $ (varibles st) M.! var

putVar :: String -> DVar -> Analyzer ()
putVar name var
  = do
      st <- get
      if M.member name (varibles st)
        then lift $ throwError (SemanticError (newPos "" 0 0) ("Variable named " ++ name ++ " already exist"))
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
      return $ DProgram 0 dProcs

-- load all procedure's prototype and check for duplicate identity
loadProcProto :: [Proc] -> Analyzer ()
loadProcProto procs
    = mapM_ (\(Proc sourcePos ident paras _ _) -> 
        do 
          -- do not check the parameter for now
          -- will be checked when analysing the procedure
          let dParas = map (\(Para _ _ baseType indi) -> (DProcProtoPara indi baseType)) paras
          pid <- getProcCounter
          putProcProto ident (DProcProto pid dParas)
          return ()
        ) procs

checkProc :: Proc -> Analyzer DProc
checkProc (Proc sourcePos ident paras decls stmts)
  = do
      resetVar
      resetSlotCounter
      (DProcProto pid _) <- getProcProto ident
      -- parameters
      dParas <- mapM (\(Para sourcePos ident baseType indi) -> 
        do
          sc <- getSlotCounter 1
          putVar ident (DVar sc (ShapeVar) baseType)
          return (DPara sc indi baseType)
        ) paras
      -- declarations
      dDecls <- mapM (\(Decl sourcePos ident baseType shape) -> 
        do
          sc <- getSlotCounter (getVarSizeByShape shape)
          putVar ident (DVar sc shape baseType)
          return (DDecl sc baseType shape)
        ) decls
      -- the current counter + 1 is the total size
      totalSize <- getSlotCounter 1

      -- statements
      dStmts <- mapM checkStat stmts

      return (DProc pid dParas dDecls dStmts totalSize)

checkStat :: Stmt -> Analyzer DStmt
-- checkStat (Assign sourcePos var expr)
checkStat _
-- palceholder
  = do
      return (DCall 0 [])

getVarSizeByShape :: Shape -> Int
getVarSizeByShape (ShapeVar) = 1
getVarSizeByShape (ShapeArr a) = a
getVarSizeByShape (ShapeMat a b) = a * b
