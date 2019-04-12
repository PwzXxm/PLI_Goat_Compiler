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

boolConst :: Parser Bool
boolConst
  = gToken (\t -> case t of {BOOL_CONST v -> Just v; other -> Nothing})

floatConst :: Parser Float
floatConst
  = gToken (\t -> case t of {FLOAT_CONST v -> Just v; other -> Nothing})

strLitConst :: Parser String
strLitConst
  = gToken (\t -> case t of {LIT v -> Just v; other -> Nothing})

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
    <|>
    do
      reserved FLOAT
      return FloatType

pDecl :: Parser Decl
pDecl
  = do
      basetype <- pBaseType
      ident <- identifier
      shape <- pShape
      reserved SEMI
      return (Decl ident basetype shape)

-- Expr
-- L1: ||
-- L2: &&
-- L3: !
-- L4: = != < <= > >=
-- L5: + -
-- L6: * /
-- L7: -

pExpr :: Parser Expr
pExpr
  = pExprL1

pExprL1 :: Parser Expr
pExprL1 = chainl1 pExprL2 pBoolOr

pExprL2 :: Parser Expr
pExprL2 = chainl1 pExprL3 pBoolAnd

pExprL3 :: Parser Expr
pExprL3 = choice [pUnaryNot, pExprL4]

pExprL4 :: Parser Expr
pExprL4 = choice [try pRelationalOps, pExprL5]

pExprL5 :: Parser Expr
pExprL5 = chainl1 pExprL6 pAddSub

pExprL6 :: Parser Expr
pExprL6 = chainl1 pExprL7 pMulDiv

pExprL7 :: Parser Expr
-- pExprL7 = choice [pUnaryMinus, parens pExpr, pBoolConst, pIntConst, pFloatConst ]
pExprL7 = choice [pUnaryMinus, pBoolConst, pIntConst, pFloatConst, pEvar, pParensExpr]

pParensExpr :: Parser Expr
pParensExpr
  = do
      reserved LPAREN
      e <- pExpr
      reserved RPAREN
      return e

pBoolConst, pIntConst, pFloatConst, pStrLit :: Parser Expr
pBoolConst
  = do
      b <- boolConst
      return (BoolConst b)

pIntConst
  = do
      i <- intConst
      return (IntConst i)

pFloatConst
  = do
      f <- floatConst
      return (FloatConst f)

pStrLit
  = do
      s <- strLitConst
      return (StrConst s)
 
--pAdd, pSub, pMul, pDiv, pAnd, pOr :: Parser (Expr -> Expr -> Expr)
pAddSub :: Parser (Expr -> Expr -> Expr)

pAddSub
  = do
      reserved MINUS
      return (BinaryOp Op_sub)
    <|>
    do
      reserved PLUS
      return (BinaryOp Op_add)

pMulDiv
  = do
      reserved MUL
      return (BinaryOp Op_mul)
    <|>
    do
      reserved DIV
      return (BinaryOp Op_div)


pBoolAnd
  = do
      reserved AND
      return (BinaryOp Op_and)

pBoolOr
  = do
      reserved OR
      return (BinaryOp Op_or)


pRelationalOps :: Parser Expr
pRelationalOps
  = do
      f1 <- pExprL5
      op <- pRelationalOperator
      f2 <- pExprL5
      return (BinaryOp op f1 f2)


pRelationalOperator :: Parser Binop
pRelationalOperator
  = do
      reserved EQUAL
      return Op_eq
    <|>
    do
      reserved UNEQUAL
      return Op_ne
    <|>
    do
      reserved LESS
      return Op_lt
    <|>
    do
      reserved LESSEQUAL
      return Op_le
    <|>
    do
      reserved GREATER
      return Op_gt
    <|>
    do
      reserved GREATEQUAL
      return Op_ge

pUnaryMinus, pUnaryNot :: Parser Expr
pUnaryMinus
  = do
      reserved MINUS
      f <- pExprL7
      return (UnaryMinus f)

pUnaryNot
  = do
      reserved UNARYNOT
      f <- pExprL4
      return (UnaryNot f)

pEvar :: Parser Expr
pEvar
  = do
      v <- pVar
      return (Evar v)

-- Expr End

pShape :: Parser Shape
pShape
  = do
      reserved LSQUARE
      s0 <- intConst
      shape <- (
        do
          reserved RSQUARE
          return (ShapeArr s0)
        <|>
        do
          reserved COMMA
          s1 <- intConst
          reserved RSQUARE
          return (ShapeMat s0 s1))
      return shape
    <|>
    do
      return ShapeVar

pIdx :: Parser Idx
pIdx
  = do
      reserved LSQUARE
      e0 <- pExpr
      shape <- (
        do
          reserved RSQUARE
          return (IdxArr e0)
        <|>
        do
          reserved COMMA
          e1 <- pExpr
          reserved RSQUARE
          return (IdxMat e0 e1))
      return shape
    <|>
    do
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
      e <- choice [pStrLit, pExpr]
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
  = choice [pIf, pWhile]

pIf, pWhile :: Parser Stmt
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

pWhile
  = do
      reserved WHILE
      e <- pExpr
      reserved DO
      stmts <- many1 pStmt
      reserved OD
      return (While e stmts)

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

runGoatParser :: [Token] -> Either ParseError GoatProgram
runGoatParser tokens = runParser pMain () "" tokens

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
        Right ast -> runGoatFormatterAndOutput ast
        Left  err -> print err
      