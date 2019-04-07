module GoatToken where

import Text.Parsec.Pos

type Token = (SourcePos, Tok)

data Tok
  = BOOL | INT | FLOAT | PROC | BEGIN | END | READ | WRITE | ELSE | IF
  | FI | DO | OD | VAL | WHILE | REF | THEN | ASSIGN 
  | INT_CONST Int | BOOL_CONST Bool | FLOAT Float | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
  deriving (Eq, Show)
    