-- | Parse given tokens and returns the Abstract Syntax Tree
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatParser(runGoatParser) where

import           GoatAST
import           GoatFormatter
import           GoatLexer
import           GoatToken

import           Data.Char
import           System.Environment
import           System.Exit
import           Text.Parsec
import           Text.Parsec.Pos

type Parser a = Parsec [Token] () a

-----------------------------------------------------------------
--  Parser helpers for recognizing a specific token (and its value)
-----------------------------------------------------------------

-- | General helper
gToken :: (Tok -> Maybe a) -> Parser a
gToken test
  = token showToken posToken testToken
    where
      showToken (pos, tok) = show tok
      posToken  (pos, tok) = pos
      testToken (pos, tok) = test tok

-- | Parser for tokens that don't have values
reserved :: Tok -> Parser ()
reserved tok
  = gToken (\t -> if t == tok then Just () else Nothing)

-- | Parsers for tokens that have values
identifier :: Parser String
identifier
  = do
      gToken (\t -> case t of {IDENT id -> Just id; other -> Nothing})
    <?>
    "identifier"

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

-----------------------------------------------------------------


-----------------------------------------------------------------
--  pExpr is the main parser for expressions. 
--  Level is decided based on the precedence of each operator
--  pStrLit is not a part of pExpr, it is only used in write statement
-----------------------------------------------------------------
--  pExprL1: ||
--  pExprL2: &&
--  pExprL3: !
--  pExprL4: = != < <= > >=
--  pExprL5: + -
--  pExprL6: * /
--  pExprL7: -
-----------------------------------------------------------------

pExpr :: Parser Expr
pExpr
  = do
      pExprL1
    <?>
    "expression"

pExprL1 :: Parser Expr
pExprL1 = chainl1 pExprL2 pBoolOr

pExprL2 :: Parser Expr
pExprL2 = chainl1 pExprL3 pBoolAnd

pExprL3 :: Parser Expr
pExprL3 = choice [pUnaryNot, pExprL4]

-- | Relational operators are non-associative
pExprL4 :: Parser Expr
pExprL4 = choice [try pRelationalOps, pExprL5] 

pExprL5 :: Parser Expr
pExprL5 = choice [pBoolConst, (chainl1 pExprL6 pAddSub)]

pExprL6 :: Parser Expr
pExprL6 = chainl1 pExprL7 pMulDiv

pExprL7 :: Parser Expr
pExprL7 = choice [pUnaryMinus, pIntConst, pFloatConst, pEvar, pParensExpr]

pParensExpr :: Parser Expr
pParensExpr
  = do
      reserved LPAREN
      e <- pExpr
      reserved RPAREN
      return e
    <?>
    "(<expression>)"

pBoolConst, pIntConst, pFloatConst, pStrLit :: Parser Expr
pBoolConst
  = do
      pos <- getPosition
      b <- boolConst
      return (BoolConst pos b)
    <?>
    "bool const"

pIntConst
  = do
      pos <- getPosition
      i <- intConst
      return (IntConst pos i)
    <?>
    "int const"

pFloatConst
  = do
      pos <- getPosition
      f <- floatConst
      return (FloatConst pos f)
    <?>
    "float const"

-- | pStrLit is not a part of pExpr, it is only used in write statement
pStrLit
  = do
      pos <- getPosition
      s <- strLitConst
      return (StrConst pos s)
    <?>
    "string literal"


pAddSub :: Parser (Expr -> Expr -> Expr)

pAddSub
  = do
      pos <- getPosition
      reserved MINUS
      return (BinaryOp pos Op_sub)
    <|>
    do
      pos <- getPosition
      reserved PLUS
      return (BinaryOp pos Op_add)

pMulDiv
  = do
      pos <- getPosition
      reserved MUL
      return (BinaryOp pos Op_mul)
    <|>
    do
      pos <- getPosition
      reserved DIV
      return (BinaryOp pos Op_div)

pBoolAnd
  = do
      pos <- getPosition
      reserved AND
      return (BinaryOp pos Op_and)

pBoolOr
  = do
      pos <- getPosition
      reserved OR
      return (BinaryOp pos Op_or)

pRelationalOps :: Parser Expr
pRelationalOps
  = do
      pos <- getPosition
      f1 <- pExprL5
      op <- pRelationalOperator
      f2 <- pExprL5
      return (BinaryOp pos op f1 f2)

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
      pos <- getPosition
      reserved MINUS
      f <- pExprL7
      return (UnaryMinus pos f)

pUnaryNot
  = do
      pos <- getPosition
      reserved UNARYNOT
      f <- pExprL4
      return (UnaryNot pos f)

pEvar :: Parser Expr
pEvar
  = do
      pos <- getPosition
      v <- pVar
      return (Evar pos v)
    <?>
    "variable"

-----------------------------------------------------------------


-----------------------------------------------------------------
--  Parsers for variables / declearations / parameters
-----------------------------------------------------------------

-- | Parser for variable declaration
pDecl :: Parser Decl
pDecl
  = do
      pos <- getPosition
      basetype <- pBaseType
      ident <- identifier
      shape <- pShape
      reserved SEMI
      return (Decl pos ident basetype shape)

-- | Parser for shape of variable in declaration
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

-- | Parse variables: identifier, array and matrix
pVar :: Parser Var
pVar
  = do
      pos <- getPosition
      ident <- identifier
      idx <- pIdx
      return (Var pos ident idx)

-- | Parser for variable index
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

-- | Parser for variable type
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
    <?>
    "type"

-- | Parser for parameters of procdures
pPara :: Parser Para
pPara
  = do
      pos <- getPosition
      indi <- pParaIndi
      t <- pBaseType
      id <- identifier
      return (Para pos id t indi)

pParaIndi :: Parser Indi
pParaIndi
  = do
      reserved VAL
      return InVal
    <|>
    do
      reserved REF
      return InRef
    <?>
    "parameter indicator"

-----------------------------------------------------------------

-----------------------------------------------------------------
--  pStmt is the main parser for statment. 
-----------------------------------------------------------------

pStmt, pStmtAtom, pStmtComp :: Parser Stmt
pStmt
  = choice [pStmtAtom, pStmtComp]

-- | parser for atomic statements
-- Including read, write, call and assignment
pStmtAtom
  = do
      r <- choice [pRead, pWrite, pCall, pAsg]
      reserved SEMI
      return r
    <?>
      "atomic statement"

pRead, pWrite, pCall, pAsg :: Parser Stmt
pRead
  = do
      pos <- getPosition
      reserved READ
      var <- pVar
      return (Read pos var)

pWrite
  = do
      pos <- getPosition
      reserved WRITE
      e <- choice [pStrLit, pExpr]
      return (Write pos e)

pCall
  = do
      pos <- getPosition
      reserved CALL
      id <- identifier
      reserved LPAREN
      exprs <- sepBy pExpr (reserved COMMA)
      reserved RPAREN
      return (Call pos id exprs)

pAsg
  = do
      pos <- getPosition
      v <- pVar
      reserved ASSIGN
      e <- pExpr
      return (Assign pos v e)


-- | Parser for composite statements
-- Including read, write, call and assignment
pStmtComp = (choice [pIf, pWhile]) <?> "composite statement"

pIf, pWhile :: Parser Stmt
pIf
  = do
      pos <- getPosition
      reserved IF
      e <- pExpr
      reserved THEN
      stmts <- many1 pStmt
      -- check if there is an else statment
      -- if not, return empty
      estmts <- (
        do
          reserved FI
          return []
        <|>
        do
          reserved ELSE
          -- else body can not be empty
          s <- many1 pStmt
          reserved FI
          return s)

      return (If pos e stmts estmts)

pWhile
  = do
      pos <- getPosition
      reserved WHILE
      e <- pExpr
      reserved DO
      stmts <- many1 pStmt
      reserved OD
      return (While pos e stmts)

-----------------------------------------------------------------

-- | Parser for the procedure
pProc :: Parser Proc
pProc
  = do
      pos <- getPosition
      reserved PROC
      id <- identifier
      reserved LPAREN
      paras <- sepBy pPara (reserved COMMA)
      reserved RPAREN
      decls <- many pDecl
      reserved BEGIN
      stmts <- many1 pStmt
      reserved END
      return (Proc pos id paras decls stmts)
    <?>
    "procedure"

-- | Parse and check there is a least one procedure
-- and it reaches the end of file
pMain :: Parser GoatProgram
pMain
  = do
      procs <- many1 pProc
      eof
      return (Program procs)

-- | Perform parsing for Goat language
-- It takes a list of tokens recognized at the lexical analysis stage
-- It returns the abstract syntax tree if successful, of type 'GoatProgram'
-- Otherwise, returns a error, of type 'ParseError'
runGoatParser :: [Token] -> Either ParseError GoatProgram
runGoatParser tokens = runParser pMain () "" tokens
