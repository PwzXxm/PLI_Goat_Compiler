module GoatFormatter where

import GoatAST

runGoatFormatter :: GoatProgram -> IO ()
runGoatFormatter (Program []) = return ()
runGoatFormatter (Program (x:xs))
  = do
      programFormatter x
      runGoatFormatter (Program xs)

programFormatter :: Proc -> IO ()
programFormatter (Proc id paras decls stmts)
  = do
      putStr "proc "
      putStr id
      putStr "("
      parasFormatter paras
      putStr ")\n"
      declsFormatter decls
      putStr "begin\n"
      stmtsFormatter stmts
      putStr "end\n"


parasFormatter :: [Para] -> IO ()
parasFormatter [x] = paraFormatter x
parasFormatter (x:xs)
  = do
      paraFormatter x
      putStr ", "
      parasFormatter xs

paraFormatter :: Para -> IO ()
paraFormatter (Para id btype indi)
  = do
      indiFormatter indi
      putStr " "
      btypeFormatter btype
      putStr " "
      putStr id

declsFormatter :: [Decl] -> IO ()
declsFormatter [] = return ()
declsFormatter ((Decl id btype shape):xs)
  = do
      putStr "    "
      btypeFormatter btype
      putStr " "
      putStr id
      shapeFormatter shape
      putStr "\n"
      declsFormatter xs
      
stmtsFormatter :: [Stmt] -> IO ()
stmtsFormatter [] = return ()
stmtsFormatter (x:xs)
  = do
      stmtFormatter 1 x
      putStr "\n"
      stmtsFormatter xs

stmtFormatter :: Int -> Stmt -> IO ()
stmtFormatter inde (Read var)
  = do
      indeFormatter inde
      putStr "read "
      varFormatter var
stmtFormatter inde (Write expr)
  = do
      indeFormatter inde
      putStr "write "
      exprFormatter expr

indiFormatter :: Indi -> IO ()
indiFormatter InVar = putStr "var"
indiFormatter InRef = putStr "ref"

btypeFormatter :: BaseType -> IO ()
btypeFormatter BoolType = putStr "bool"
btypeFormatter IntType = putStr "int"

shapeFormatter :: Shape -> IO ()
shapeFormatter ShapeVar = return ()
shapeFormatter (ShapeArr a)
  = do
    putStr "["
    putStr (show a)
    putStr "]"
shapeFormatter (ShapeMat a b)
  = do
    putStr "["
    putStr (show a)
    putStr ", "
    putStr (show b)
    putStr "]"

varFormatter :: Var -> IO ()
varFormatter (Var id idx)
  = do
      putStr id
      idxFormatter idx

idxFormatter :: Idx -> IO ()
idxFormatter IdxVar = return ()
idxFormatter (IdxArr expr)
  = do
      putStr "["
      exprFormatter expr
      putStr "]"
idxFormatter (IdxMat e1 e2)
  = do
      putStr "["
      exprFormatter e1
      putStr "]["
      exprFormatter e2
      putStr "]"

exprFormatter :: Expr -> IO ()
exprFormatter (BoolConst bool) = putStr (show bool)
exprFormatter (IntConst int) = putStr (show int)
exprFormatter (StrConst string) = putStr (show string)
exprFormatter (Id id) = putStr id
exprFormatter (Add e1 e2)
  = do
      exprFormatter e1
      putStr " + "
      exprFormatter e2
exprFormatter (Mul e1 e2)
  = do
      exprFormatter e1
      putStr " * "
      exprFormatter e2
exprFormatter (UnaryMinus expr)
  = do
      putStr "-"
      exprFormatter expr


indeFormatter :: Int -> IO ()
indeFormatter 0 = return ()
indeFormatter a
  = do
    putStr "    "
    indeFormatter (a-1)