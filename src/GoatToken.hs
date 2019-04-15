-- | Data structure for the token list
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatToken where

import           Text.Parsec.Pos

type Token = (SourcePos, Tok)

data Tok
  = BOOL
  | INT
  | FLOAT
  | PROC
  | BEGIN
  | END
  | READ
  | WRITE
  | ELSE
  | IF
  | FI
  | DO
  | OD
  | VAL
  | WHILE
  | REF
  | THEN
  | CALL
  | ASSIGN
  | INT_CONST Int
  | BOOL_CONST Bool
  | FLOAT_CONST Float
  | IDENT String
  | LIT String
  | LPAREN
  | RPAREN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LESS
  | LESSEQUAL
  | GREATER
  | GREATEQUAL
  | EQUAL
  | UNEQUAL
  | LSQUARE
  | RSQUARE
  | OR
  | AND
  | UNARYNOT
  | SEMI
  | COMMA
  | UNKNOWN String
  deriving (Eq)

-- | Override show to have a more user friendly error message
instance Show Tok where
  show BOOL               = "boolean"
  show INT                = "int"
  show FLOAT              = "float"
  show PROC               = "proc"
  show BEGIN              = "begin"
  show END                = "end"
  show READ               = "read"
  show WRITE              = "write"
  show ELSE               = "else"
  show IF                 = "if"
  show FI                 = "fi"
  show DO                 = "do"
  show OD                 = "od"
  show VAL                = "val"
  show WHILE              = "while"
  show REF                = "ref"
  show THEN               = "then"
  show CALL               = "call"
  show ASSIGN             = "assign"
  show (INT_CONST x)      = "int const" ++ userInputWrapper (show x)
  show (BOOL_CONST True)  = "bool const" ++ userInputWrapper "true"
  show (BOOL_CONST False) = "bool const" ++ userInputWrapper "false"
  show (FLOAT_CONST x)    = "float const" ++ userInputWrapper (show x)
  show (IDENT x)          = "identifier" ++ userInputWrapper x
  show (LIT x)            = "string literal" ++ userInputWrapper x
  show LPAREN             = "("
  show RPAREN             = ")"
  show PLUS               = "+"
  show MINUS              = "-"
  show MUL                = "*"
  show DIV                = "/"
  show LESS               = "<"
  show LESSEQUAL          = "<="
  show GREATER            = ">"
  show GREATEQUAL         = ">="
  show EQUAL              = "="
  show UNEQUAL            = "!="
  show LSQUARE            = "["
  show RSQUARE            = "]"
  show OR                 = "||"
  show AND                = "&&"
  show UNARYNOT           = "!"
  show SEMI               = ";"
  show COMMA              = ","
  show (UNKNOWN x)        = "unknown token" ++ userInputWrapper x

userInputWrapper :: String -> String
userInputWrapper str = ": \"" ++ str ++ "\""
