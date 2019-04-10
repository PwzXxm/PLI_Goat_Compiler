module GoatParser where

import GoatAST
import GoatToken
import GoatLexer

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
  = gToken (\t -> case t of 
                    IDENT id -> Just id
                    other -> Nothing)

intConst :: Parser Int
intConst
  = gToken (\t -> case t of 
                    INT_CONST v -> Just v
                    other -> Nothing)

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



pProc :: Parser Proc
pProc
  = do
      reserved PROC
      id <- identifier
      reserved LPAREN
      reserved RPAREN
      decls <- many pDecl
      reserved BEGIN
      stmts <- many1 pStmt
      reserved END

      return (Proc id [] decls stmts)


-- pStmt, pRead, pWrite, pAsg :: Parser Stmt
pStmt, pRead :: Parser Stmt
pStmt 
  = choice [pRead]

pRead
  = do
      reserved READ
      var <- pVar
      reserved SEMI
      return (Read var)


-- pAsg
--   = do
--       lvalue <- pLvalue
--       reserved ASSIGN
--       rvalue <- pExp
--       return (Assign lvalue (IntConst 1))



pVar :: Parser Var
pVar
  = do
      ident <- identifier
      return (Var ident IdxVar)


-- TODO: need to check EOF
pMain :: Parser GoatProgram
pMain
  = do
      procs <- many1 pProc
      eof
      return (Program procs)


-- testdata1 = [(0,PROC), (0,IDENT "main"),(0,LPAREN),(0,RPAREN),(0,INT),(0,IDENT "a"),(0,SEMI),(0,INT),(0,IDENT "b"),(0,SEMI),(0,BEGIN),(0,IDENT "a"),(0,ASSIGN),(0,INT_CONST 2),(0,MUL),(0,LPAREN),(0,INT_CONST 1),(0,PLUS),(0,INT_CONST 10),(0,RPAREN),(0,PLUS),(0,INT_CONST 2),(0,PLUS),(0,INT_CONST 2),(0,MUL),(0,INT_CONST 2),(0,PLUS),(0,INT_CONST 14),(0,SEMI),(0,IDENT "b"),(0,ASSIGN),(0,MINUS),(0,IDENT "a"),(0,PLUS),(0,IDENT "a"),(0,SEMI),(0,END)]
-- testdata2 = [(0,RPAREN),(0,INT),(0,IDENT "a")]
-- test1 = runParser pMain () "" testdata1
-- test2 = runParser pMain () "" testdata2

test
  = do
      input <- readFile "test.in"
      let tokens = runGoatLexer "test.in" input
      let res = runParser pMain () "" tokens
      return res

