-- | Pretty printing for parser checking
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatFormatter
( runGoatFormatterAndOutput
, runGoatFormatterAndReturnString
) where

import           GoatAST

import           Numeric

-- Writer used for IO like output with higher performance than (++)
-- From https://kseo.github.io/posts/2017-01-21-writer-monad.html
import           Control.Monad.Trans.Writer.Strict
import           Data.Monoid

output :: String -> Writer (Endo [String]) ()
output x = tell $ Endo ([x]<>)

type StrWriter = Writer (Endo [String]) ()
----------------------------------------------

-- | Concatenate strings into one
runGoatFormatterAndReturnString :: GoatProgram -> String
runGoatFormatterAndReturnString program
  = concat (appEndo (execWriter (runGoatFormatter program)) [])

-- | Print the output
runGoatFormatterAndOutput :: GoatProgram -> IO ()
runGoatFormatterAndOutput program
  = mapM_ putStr (appEndo (execWriter (runGoatFormatter program)) [])

-- | Print programs in turn
runGoatFormatter :: GoatProgram -> StrWriter
runGoatFormatter (Program [x]) = programFormatter x
runGoatFormatter (Program (x:xs))
  = do
      programFormatter x
      output "\n"
      runGoatFormatter (Program xs)

-- | Print one program
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

-- | Print parameters in the program definition in turn
parasFormatter :: [Para] -> StrWriter
parasFormatter [] = return ()
parasFormatter [x] = paraFormatter x
parasFormatter (x:xs)
  = do
      paraFormatter x
      output ", "
      parasFormatter xs

-- | Print one parameter
paraFormatter :: Para -> StrWriter
paraFormatter (Para id btype indi)
  = do
      indiFormatter indi
      output " "
      btypeFormatter btype
      output " "
      output id

-- | Print declarations in the program
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

-- | Print statements in the program
-- | Int: indention of the line
stmtsFormatter :: Int -> [Stmt] -> StrWriter
stmtsFormatter _ [] = return ()
stmtsFormatter inde (x:xs)
  = do
      stmtFormatter inde x
      output "\n"
      stmtsFormatter inde xs

-- | Print one statement depending on its structure
-- | Int: indention of the line
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

-- | Print indication in presentation of the parameter
indiFormatter :: Indi -> StrWriter
indiFormatter InVar = output "var"
indiFormatter InRef = output "ref"

-- | Print base type of the identification
btypeFormatter :: BaseType -> StrWriter
btypeFormatter BoolType  = output "bool"
btypeFormatter IntType   = output "int"
btypeFormatter FloatType = output "float"

-- | Print shape of the array, in 1d or 2d.
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

-- | Print one varible
varFormatter :: Var -> StrWriter
varFormatter (Var id idx)
  = do
      output id
      idxFormatter idx

-- | Print index of array, in 1d or 2d
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

-- | Print expressions in the program
exprsFormatter :: [Expr] -> StrWriter
exprsFormatter [] = return ()
exprsFormatter [x] = exprFormatter False x
exprsFormatter (x:xs)
  = do
      exprFormatter False x
      output ", "
      exprsFormatter xs

-- | Print one expression depending on its structure
-- | Bool: True: There is a pair of parenthesises out of the expression
-- |       Flase: Vice versa
exprFormatter :: Bool -> Expr -> StrWriter
exprFormatter _ (BoolConst True) = output "true"
exprFormatter _ (BoolConst False) = output "false"
exprFormatter _ (IntConst int) = output (show int)
exprFormatter _ (FloatConst float) = output (showFFloatAlt Nothing float "")
exprFormatter _ (StrConst string) = output ("\"" ++ string ++ "\"")
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

-- | Print one binary operation
binopFotmatter :: Binop -> StrWriter
binopFotmatter Op_add = output " + "
binopFotmatter Op_sub = output " - "
binopFotmatter Op_mul = output " * "
binopFotmatter Op_div = output " / "
binopFotmatter Op_eq  = output " = "
binopFotmatter Op_ne  = output " != "
binopFotmatter Op_lt  = output " < "
binopFotmatter Op_le  = output " =< "
binopFotmatter Op_gt  = output " > "
binopFotmatter Op_ge  = output " >= "
binopFotmatter Op_and = output " && "
binopFotmatter Op_or  = output " || "

-- | Print indention of the line
indeFormatter :: Int -> StrWriter
indeFormatter 0 = return ()
indeFormatter a
  = do
    output "    "
    indeFormatter (a-1)
