module GoatToken where

import Text.Parsec.Pos

type Token = (SourcePos, Tok)

data Tok
  = BOOL | INT | FLOAT | PROC | BEGIN | END | READ | WRITE | ELSE | IF
  | FI | DO | OD | VAL | WHILE | REF | THEN | CALL | ASSIGN | INT_CONST Int 
  | BOOL_CONST Bool | FLOAT_CONST Float | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | DIV | LESS | LESSEQUAL | GREATER 
  | GREATEQUAL | EQUAL | UNEQUAL | LSQUARE | RSQUARE | OR | AND | UNARY | SEMI 
  deriving (Eq, Show)
  