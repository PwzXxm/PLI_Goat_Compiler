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

runGoatFormatterAndReturnString :: GoatProgram -> String
runGoatFormatterAndReturnString program
  = concat (appEndo (execWriter (runGoatFormatter program)) [])

runGoatFormatterAndOutput :: GoatProgram -> IO ()
runGoatFormatterAndOutput program
  = mapM_ putStr (appEndo (execWriter (runGoatFormatter program)) [])

runGoatFormatter :: GoatProgram -> StrWriter
runGoatFormatter (Program []) = return ()
runGoatFormatter (Program (x:xs))
  = do
      programFormatter x
      output "\n"
      runGoatFormatter (Program xs)

programFormatter :: Proc -> StrWriter
programFormatter (Proc id paras decls stmts)
  = do
      output "proc "
      output id
      output " ("
      parasFormatter paras
      output ")\n"
      declsFormatter decls
      output "begin\n"
      stmtsFormatter 1 stmts
      output "end\n"


parasFormatter :: [Para] -> StrWriter
parasFormatter [] = return ()
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

declsFormatter :: [Decl] -> StrWriter
declsFormatter [] = return ()
declsFormatter ((Decl id btype shape):xs)
  = do
      output "    "
      btypeFormatter btype
      output " "
      output id
      shapeFormatter shape
      output ";\n"
      declsFormatter xs
      
stmtsFormatter :: Int -> [Stmt] -> StrWriter
stmtsFormatter _ [] = return ()
stmtsFormatter inde (x:xs)
  = do
      stmtFormatter inde x
      output "\n"
      stmtsFormatter inde xs

stmtFormatter :: Int -> Stmt -> StrWriter
stmtFormatter inde (Read var)
  = do
      indeFormatter inde
      output "read "
      varFormatter var
      output ";"
stmtFormatter inde (Write expr)
  = do
      indeFormatter inde
      output "write "
      exprFormatter False expr
      output ";"
stmtFormatter inde (Assign var expr)
  = do
      indeFormatter inde
      varFormatter var
      output " := "
      exprFormatter False expr
      output ";"
stmtFormatter inde (Call id xs)
  = do
      indeFormatter inde
      output "call "
      output id
      output "("
      exprsFormatter xs
      output ")"
      output ";"
stmtFormatter inde (If expr s1 [])
  = do
      indeFormatter inde
      output "if "
      exprFormatter False expr
      output " then\n"
      stmtsFormatter (inde+1) s1
      indeFormatter inde
      output "fi"
stmtFormatter inde (If expr s1 s2)
  = do
      indeFormatter inde
      output "if "
      exprFormatter False expr
      output " then\n"
      stmtsFormatter (inde+1) s1
      indeFormatter inde
      output "else\n"
      stmtsFormatter (inde+1) s2
      indeFormatter inde
      output "fi"
stmtFormatter inde (While expr stmts)
  = do
      indeFormatter inde
      output "while "
      exprFormatter False expr
      output " do\n"
      stmtsFormatter (inde+1) stmts
      indeFormatter inde
      output "od"

indiFormatter :: Indi -> StrWriter
indiFormatter InVar = output "var"
indiFormatter InRef = output "ref"

btypeFormatter :: BaseType -> StrWriter
btypeFormatter BoolType = output "bool"
btypeFormatter IntType = output "int"
btypeFormatter FloatType = output "float"

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

varFormatter :: Var -> StrWriter
varFormatter (Var id idx)
  = do
      output id
      idxFormatter idx

idxFormatter :: Idx -> StrWriter
idxFormatter IdxVar = return ()
idxFormatter (IdxArr expr)
  = do
      output "["
      exprFormatter False expr
      output "]"
idxFormatter (IdxMat e1 e2)
  = do
      output "["
      exprFormatter False e1
      output ", "
      exprFormatter False e2
      output "]"

exprsFormatter :: [Expr] -> StrWriter
exprsFormatter [] = return ()
exprsFormatter [x] = exprFormatter False x
exprsFormatter (x:xs)
  = do
      exprFormatter False x
      output ", "
      exprsFormatter xs

exprFormatter :: Bool -> Expr -> StrWriter
exprFormatter _ (BoolConst bool) = output (show bool)
exprFormatter _ (IntConst int) = output (show int)
exprFormatter _ (FloatConst float) = output (show float)
exprFormatter _ (StrConst string) = output (show string)
exprFormatter _ (Evar var) = varFormatter var
exprFormatter True (BinaryOp binop e1 e2)
  = do
      output "("
      exprFormatter True e1
      binopFotmatter binop
      exprFormatter True e2
      output ")"
exprFormatter False (BinaryOp binop e1 e2)
  = do
      exprFormatter True e1
      binopFotmatter binop
      exprFormatter True e2
exprFormatter _ (UnaryMinus expr)
  = do
      output "-"
      exprFormatter True expr
exprFormatter _ (UnaryNot expr)
  = do
      output "!"
      exprFormatter True expr


binopFotmatter :: Binop -> StrWriter
binopFotmatter Op_add = output " + "
binopFotmatter Op_sub = output " - "
binopFotmatter Op_mul = output " * "
binopFotmatter Op_div = output " / "
binopFotmatter Op_eq = output " = "
binopFotmatter Op_ne = output " != "
binopFotmatter Op_lt = output " < "
binopFotmatter Op_le = output " =< "
binopFotmatter Op_gt = output " > "
binopFotmatter Op_ge = output " >= "
binopFotmatter Op_and = output " && "
binopFotmatter Op_or = output " || "

indeFormatter :: Int -> StrWriter
indeFormatter 0 = return ()
indeFormatter a
  = do
    output "    "
    indeFormatter (a-1)