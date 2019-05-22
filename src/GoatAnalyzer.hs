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
import Debug.Trace

data Astate = Astate
  { procs :: M.Map String DProcProto
  , varibles :: M.Map String DVar
  , slotCounter :: Int
  , procCounter :: Int
  }

type Analyzer a = State Astate a

getProcProto :: String -> Analyzer DProcProto
getProcProto name
  = do
      st <- get
      return $ (procs st) M.! name

putProcProto :: String -> DProcProto -> Analyzer ()
putProcProto name dProcProto
  = do
      st <- get
      let p = if M.member name (procs st)
                then error ("Procedure named " ++ name ++ " already exist")
                else M.insert name dProcProto (procs st)
        in put st{procs = p}

getVar :: String -> Analyzer DVar
getVar var
  = do
      st <- get
      return $ (varibles st) M.! var

putVar :: String -> DVar -> Analyzer ()
putVar name var
  = do
      st <- get
      let v = if M.member name (varibles st)
                then error ("Variable named " ++ name ++ " already exist")
                else M.insert name var (varibles st)
        in put st{varibles = v}

getSlotCounter :: Analyzer Int
getSlotCounter
  = do
      st <- get
      put st{slotCounter = (slotCounter st) + 1}
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

semanticCheckDGoatProgram :: GoatProgram -> Analyzer DGoatProgram
semanticCheckDGoatProgram (Program procs)
  = do
      loadProcProto procs
      dProcs <- mapM checkProc procs
      return $ DProgram 0 dProcs

loadProcProto :: [Proc] -> Analyzer ()
loadProcProto procs
    = mapM_ (\(Proc sourcePos ident paras _ _) -> 
          do 
            -- do not check the parameter for now
            -- will be checked when analysing the procedure
            let dParas = map (\(Para _ _ baseType indi) -> (DPara indi baseType)) paras
            pid <- getProcCounter
            putProcProto ident (DProcProto pid dParas)
            return ()) procs
      

checkProc :: Proc -> Analyzer DProc
checkProc (Proc sourcePos ident paras decls stmts)
  = do
      (DProcProto pid _) <- getProcProto ident
      return (DProc pid [] [] [] 0)
