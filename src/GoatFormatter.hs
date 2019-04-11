module GoatFormatter where

import GoatAST

-- Writer used for IO like output with higher performance than (++)
-- From https://kseo.github.io/posts/2017-01-21-writer-monad.html
import Control.Monad.Trans.Writer.Strict
import Data.Monoid

output :: String -> Writer (Endo [String]) ()
output x = tell $ Endo ([x]<>)

type StrWriter = Writer (Endo [String]) ()
----------------------------------------------


runGoatFormatterAndOutput :: GoatProgram -> IO ()
runGoatFormatterAndOutput program
  = mapM_ putStr $ appEndo (execWriter (runGoatFormatter program)) []

runGoatFormatter :: GoatProgram -> StrWriter
runGoatFormatter (Program []) = return ()
runGoatFormatter (Program (x:xs))
  = do
      programFormatter x
      runGoatFormatter (Program xs)

programFormatter :: Proc -> StrWriter
programFormatter (Proc id paras decls stmts)
  = do
      output "proc "
      output id
      output "("
      parasFormatter paras
      output ")\n"
      -- declsFormatter decls
      -- putStr "begin\n"
      -- stmtsFormatter stmts
      -- putStr "end\n"


parasFormatter :: [Para] -> StrWriter
parasFormatter [x] = paraFormatter x
parasFormatter (x:xs)
  = do
      paraFormatter x
      output ", "
      parasFormatter xs

paraFormatter :: Para -> StrWriter
paraFormatter (Para id btype indi)
  = do
      indiFormatter indi
      output " "
      btypeFormatter btype
      output " "
      output id

-- declsFormatter :: [Decl] -> IO ()
-- declsFormatter [] = return ()
-- declsFormatter ((Decl id btype shape):xs)
--   = do
--       putStr "    "
--       btypeFormatter btype
--       putStr " "
--       putStr id
--       shapeFormatter shape
--       putStr "\n"
--       declsFormatter xs
      
-- stmtsFormatter :: [Stmt] -> IO ()
-- stmtsFormatter [] = return ()
-- stmtsFormatter (x:xs)
--   = do
--       stmtFormatter 1 x
--       putStr "\n"
--       stmtsFormatter xs

-- stmtFormatter :: Int -> Stmt -> IO ()
-- stmtFormatter inde (Read var)
--   = do
--       indeFormatter inde
--       putStr "read "
--       varFormatter var
-- stmtFormatter inde (Write expr)
--   = do
--       indeFormatter inde
--       putStr "write "
--       exprFormatter expr

indiFormatter :: Indi -> StrWriter
indiFormatter InVar = output "var"
indiFormatter InRef = output "ref"

btypeFormatter :: BaseType -> StrWriter
btypeFormatter BoolType = output "bool"
btypeFormatter IntType = output "int"

shapeFormatter :: Shape -> StrWriter
shapeFormatter ShapeVar = return ()
shapeFormatter (ShapeArr a)
  = do
    output "["
    output (show a)
    output "]"
shapeFormatter (ShapeMat a b)
  = do
    output "["
    output (show a)
    output ", "
    output (show b)
    output "]"

-- varFormatter :: Var -> IO ()
-- varFormatter (Var id idx)
--   = do
--       putStr id
--       idxFormatter idx

-- idxFormatter :: Idx -> IO ()
-- idxFormatter IdxVar = return ()
-- idxFormatter (IdxArr expr)
--   = do
--       putStr "["
--       exprFormatter expr
--       putStr "]"
-- idxFormatter (IdxMat e1 e2)
--   = do
--       putStr "["
--       exprFormatter e1
--       putStr "]["
--       exprFormatter e2
--       putStr "]"

-- exprFormatter :: Expr -> IO ()
-- exprFormatter (BoolConst bool) = putStr (show bool)
-- exprFormatter (IntConst int) = putStr (show int)
-- exprFormatter (StrConst string) = putStr (show string)
-- exprFormatter (Id id) = putStr id
-- exprFormatter (Add e1 e2)
--   = do
--       exprFormatter e1
--       putStr " + "
--       exprFormatter e2
-- exprFormatter (Mul e1 e2)
--   = do
--       exprFormatter e1
--       putStr " * "
--       exprFormatter e2
-- exprFormatter (UnaryMinus expr)
--   = do
--       putStr "-"
--       exprFormatter expr


-- indeFormatter :: Int -> IO ()
-- indeFormatter 0 = return ()
-- indeFormatter a
--   = do
--     putStr "    "
--     indeFormatter (a-1)