module GoatParser where

import GoatAST

import Data.Char
import Text.Parsec
import System.Environment
import System.Exit

type Parser a
   = Parsec [Token] () a

-- runGoatPaser :: ???
-- runGoatPaser = runParser pMain 0 ""


pMain :: Parser GoatProgram
pMain
  = do
    return Program
    --   p <- pProg
    --   return p


testdata = [(0,PROC),(0,IDENT "main"),(0,LPAREN),(0,RPAREN),(0,INT),(0,IDENT "a"),(0,SEMI),(0,INT),(0,IDENT "b"),(0,SEMI),(0,BEGIN),(0,IDENT "a"),(0,ASSIGN),(0,INT_CONST 2),(0,MUL),(0,LPAREN),(0,INT_CONST 1),(0,PLUS),(0,INT_CONST 10),(0,RPAREN),(0,PLUS),(0,INT_CONST 2),(0,PLUS),(0,INT_CONST 2),(0,MUL),(0,INT_CONST 2),(0,PLUS),(0,INT_CONST 14),(0,SEMI),(0,IDENT "b"),(0,ASSIGN),(0,MINUS),(0,IDENT "a"),(0,PLUS),(0,IDENT "a"),(0,SEMI),(0,END)]
test = runParser pMain () "" testdata


-- temp
-- type Token = (SourcePos, Tok)
type Token = (Int, Tok)

data Tok
  = BOOL | INT | PROC | BEGIN | END | READ | WRITE | ASSIGN 
  | INT_CONST Int | BOOL_CONST Bool | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
    deriving (Eq, Show)
