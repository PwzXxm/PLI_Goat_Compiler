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
      -- stmtsFormatter stmts


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
      
      

-- stmtsFormatter :: [Stmt] -> IO ()

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