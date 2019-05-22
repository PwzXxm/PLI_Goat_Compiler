-- | Semantic analyzer: check and add info to Decorated AST
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatAnalyzer where

import GoatAST
import qualified Data.Map as M
import Control.Monad.State

data Astate = Astate
  { procs :: M String [DPara]
  , varibles :: M String DVar
  , slotCounter :: Int
  , procCounter :: Int
  }

type Analyzer a = State Astate a

getProc :: String -> Analyzer [DPara]
getProc name
  = do
      st <- get
      return $ (procs st) ! name

putProc :: String -> [DPara] -> Analyzer ()
putproc name params
  = do
      st <- get
      let p = if M.member name (procs st)
                then error ("Procedure named " ++ name ++ " already exist")
                else M.insert name params (procs st)
      in put st{procs = p}

getVar :: String -> Analyzer DVar
getProc var
  = do
      st <- get
      return $ (varibles st) ! var

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
      put st{slotCounter = slotCounter + 1}
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
      put st{procCounter = procCounter + 1}
      return $ procCounter st

resetProcCounter :: Analyzer ()
resetProcCounter
  = do
      st <- get
      put st{procCounter = 0}

-----------------------------------

semanticCheckDGoatProgram :: GoatProgram -> Analyzer DGoatProgram
semanticCheckDGoatProgram Program procs
    = do
        