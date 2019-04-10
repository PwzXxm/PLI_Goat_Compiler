module GoatParser where

import GoatAST
import GoatToken
import GoatLexer
import GoatFormatter

import Data.Char
import Text.Parsec
import Text.Parsec.Pos
import System.Environment
import System.Exit

type Parser a
   = Parsec [Token] () a

reserved :: Tok -> Parser ()
reserved tok
  = gToken (\t -> if t == tok then Just () else Nothing)

identifier :: Parser String
identifier
  = gToken (\t -> case t of {IDENT id -> Just id; other -> Nothing})

intConst :: Parser Int
intConst
  = gToken (\t -> case t of {INT_CONST v -> Just v; other -> Nothing})

-- boolConst :: Parser Bool
-- boolConst
--   = gToken (\t -> case t of {FLOAT_CONST v -> Just v; other -> Nothing})

gToken :: (Tok -> Maybe a) -> Parser a
gToken test
  = token showToken posToken testToken
    where
      showToken (pos, tok) = show tok
      posToken  (pos, tok) = pos
      testToken (pos, tok) = test tok

pBaseType :: Parser BaseType
pBaseType
  = do
      reserved BOOL
      return BoolType
    <|>
    do 
      reserved INT
      return IntType

pDecl :: Parser Decl
pDecl
  = do
      basetype <- pBaseType
      ident <- identifier
      shape <- pShape
      reserved SEMI
      return (Decl ident basetype shape)

-- Expr

pExpr :: Parser Expr
pExpr
  = do
      i <- intConst
      return (IntConst i)

-- Expr End

pShape, pShapeVar, pShapeArr, pShapeMat :: Parser Shape
pShape
  = do choice [pShapeMat, pShapeArr, pShapeVar]

pShapeMat
  = try (do
      reserved LSQUARE
      s0 <- intConst
      reserved COMMA
      s1 <- intConst
      reserved RSQUARE
      return (ShapeMat s0 s1)
  )

pShapeArr
  = try (do
      reserved LSQUARE
      s <- intConst
      reserved RSQUARE
      return (ShapeArr s)
  )

pShapeVar
  = do
      return ShapeVar

pIdx, pIdxVar, pIdxArr, pIdxMat :: Parser Idx
pIdx
  = do choice [pIdxMat, pIdxArr, pIdxVar]

pIdxMat
  = try (do
      reserved LSQUARE
      e0 <- pExpr
      reserved COMMA
      e1 <- pExpr
      reserved RSQUARE
      return (IdxMat e0 e1)
  )

pIdxArr
  = try (do
      reserved LSQUARE
      e <- pExpr
      reserved RSQUARE
      return (IdxArr e)
  )

pIdxVar
  = do
      return IdxVar

pParaIndi :: Parser Indi
pParaIndi
  = do
      reserved VAL
      return InVar
    <|>
    do 
      reserved REF
      return InRef

pPara :: Parser Para
pPara
  = do
      indi <- pParaIndi
      t <- pBaseType
      id <- identifier
      return (Para id t indi)

pProc :: Parser Proc
pProc
  = do
      reserved PROC
      id <- identifier
      reserved LPAREN
      paras <- sepBy pPara (reserved COMMA)
      reserved RPAREN
      decls <- many pDecl
      reserved BEGIN
      stmts <- many1 pStmt
      reserved END
      return (Proc id paras decls stmts)

-- Stmt

pStmt, pStmtAtom, pStmtComp :: Parser Stmt
pStmt 
  = choice [pStmtAtom, pStmtComp]

pStmtAtom
  = do
      r <- choice [pRead, pWrite, pCall, pAsg]
      reserved SEMI
      return r

pRead, pWrite, pCall, pAsg :: Parser Stmt
pRead
  = do
      reserved READ
      var <- pVar
      return (Read var)

pWrite
  = do
      reserved WRITE
      e <- pExpr
      return (Write e)

pCall
  = do
      reserved CALL
      id <- identifier
      reserved LPAREN
      exprs <- sepBy pExpr (reserved COMMA)
      reserved RPAREN
      return (Call id exprs)

pAsg
  = do
      v <- pVar
      reserved ASSIGN
      e <- pExpr
      return (Assign v e)

pStmtComp
  = choice [pIf]

pIf :: Parser Stmt
pIf
  = do
      reserved IF
      e <- pExpr
      reserved THEN
      stmts <- many1 pStmt
      -- else 
      estmts <- (
        do
          reserved FI
          return []
        <|>
        do
          reserved ELSE
          s <- many1 pStmt
          reserved FI
          return s)

      return (If e stmts estmts)

-- Stmt End

pVar :: Parser Var
pVar
  = do
      ident <- identifier
      idx <- pIdx
      return (Var ident idx)


pMain :: Parser GoatProgram
pMain
  = do
      procs <- many1 pProc
      eof
      return (Program procs)


test
  = do
      input <- readFile "../build/test.in"
      let tokens = runGoatLexer "../build/test.in" input
      let res = runParser pMain () "" tokens
      return res

testf
  = do
      input <- readFile "../build/test.in"
      let tokens = runGoatLexer "../build/test.in" input
      let res = runParser pMain () "" tokens
      case res of
        Right ast -> runGoatFormatter ast
        Left  err -> print err
      