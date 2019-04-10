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
  -- = choice [pStrLit, (chainl1 pTerm pAdd), (chainl1 pTerm pSub), pRelationalOps]
  = choice [pStrLit, (chainl1 pFactor pAddSub)]

-- pTerm :: parser Expr
-- pTerm = choice [(chainl1 pFactor pMul), (chainl1 pFactor pDiv)]
-- 
pFactor :: Parser Expr
-- pFactor = choice [pUnaryMinus, paras pExpr, pBoolConst, pIntConst, pFloatConst ]
pFactor = choice [pBoolConst, pIntConst, pFloatConst]

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

-- pMul
--   = do
--       reserved MUL
--       return BinaryOps (Mul)
-- 
-- pDiv
--   = do
--       reserved DIV
--       return Div
-- 
-- pAnd
--   = do
--       reserved AND
--       return And
-- 
-- pOr
--   = do
--       reserved OR
--       return Or
-- 
-- pRelationalOps :: Parser (Expr -> Expr -> Expr)
-- pRelationalOps
--   = choice [pEq, pNe, pLt, pLe, pGt, pGe]
-- 
-- pEq, pNe, pLt, pLe, pGt, pGe :: Parser Expr
-- pEq
--   = do
--       f1 <- pFactor
--       reserved EQUAL
--       f2 <- pFactor
--       return (BinaryOp Op_eq f1 f2)
-- 
-- pNe
--   = do
--       f1 <- pFactor
--       reserved UNEQUAL
--       f2 <- pFactor
--       return (BinaryOp Op_ne f1 f2)
-- 
-- pLt
--   = do
--       f1 <- pFactor
--       reserved LESS
--       f2 <- pFactor
--       return (BinaryOp Op_lt f1 f2)
-- 
-- pLe
--   = do
--       f1 <- pFactor
--       reserved LESSEQUAL
--       f2 <- pFactor
--       return (BinaryOp Op_le f1 f2)
-- 
-- pGt
--   = do
--       f1 <- pFactor
--       reserved GREATER
--       f2 <- pFactor
--       return (BinaryOp Op_gt f1 f2)
-- 
-- pGe
--   = do
--       f1 <- pFactor
--       reserved GREATEQUAL
--       f2 <- pFactor
--       return (BinaryOp Op_ge f1 f2)
-- 
-- pUnaryMinus, pUnaryNot :: Parser Expr
-- pUnaryMinus
--   = do
--       reserved MINUS
--       f <- pFactor
--       return (UnaryMinus f)
-- 
-- pUnaryNot
--   = do
--       reserved UNARYNOT
--       f <- pFactor
--       return (UnaryNot f)

-- pEvar :: Parsec Expr
-- pEvar
--   = do
--       v <- pVar
--       return (Evar v)

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
      