{
module Tokens_posn (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
@float       = $digit \. $digit
@stringlit   = \" [^\"]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n

rules :-

  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { tok (\p s -> INT_CONST p (read s :: Int)) }
  @float     { tok (\p s -> FLOAT_CONST p (read s :: Float)) }
  true       { tok (\p s -> BOOL_CONST p True) }
  false      { tok (\p s -> BOOL_CONST p False) }
  bool       { tok (\p s -> BOOL p) }
  int        { tok (\p s -> INT p) }
  float      { tok (\p s -> FLOAT p) }
  proc       { tok (\p s -> PROC p) }
  begin      { tok (\p s -> BEGIN p) }
  end        { tok (\p s -> END p) }
  read       { tok (\p s -> READ p) }
  write      { tok (\p s -> WRITE p) }
  else       { tok (\p s -> ELSE p) }
  if         { tok (\p s -> IF p) }
  fi         { tok (\p s -> FI p) }
  do         { tok (\p s -> DO p) }
  od         { tok (\p s -> OD p) }
  val        { tok (\p s -> VAL p) }
  while      { tok (\p s -> WHILE p) }
  ref        { tok (\p s -> REF p) }
  then       { tok (\p s -> THEN p) }
  :=         { tok (\p s -> ASSIGN p) }
  \(         { tok (\p s -> LPAREN p) }
  \)         { tok (\p s -> RPAREN p) }
  \+         { tok (\p s -> PLUS p) }
  \-         { tok (\p s -> MINUS p) }
  \*         { tok (\p s -> MUL p) }
  \;         { tok (\p s -> SEMI p) }
  @ident     { tok (\p s -> IDENT p s) }
  @stringlit { tok (\p s -> LIT p s) }

{
data Token
  = BOOL | INT | FLOAT | PROC | BEGIN | END | READ | WRITE | ELSE | IF
  | FI | DO | OD | VAL | WHILE | REF | THEN | ASSIGN 
  | INT_CONST Int | BOOL_CONST Bool | FLOAT Float | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
    deriving (Eq, Show)

token_posn (INT_CONST p _) = p
token_posn (FLOAT_CONST p _) = p 
token_posn (BOOL_CONST p _) = p 
token_posn (BOOL p) = p 
token_posn (INT p) = p 
token_posn (FLOAT p) = p 
token_posn (PROC p) = p 
token_posn (BEGIN p) = p 
token_posn (END p) = p 
token_posn (WRITE p) = p 
token_posn (ELSE p) = p 
token_posn (IF p) = p 
token_posn (FI p) = p 
token_posn (DO p) = p 
token_posn (OD p) = p 
token_posn (WHILE p) = p 
token_posn (REF p) = p 
token_posn (THEN p) = p 
token_posn (LPAREN p) = p 
token_posn (RPAREN p) = p 
token_posn (PLUS p) = p 
token_posn (MINUS p) = p 
token_posn (MUL p) = p 
token_posn (SEMI p) = p 
token_posn (IDENT p _) = p 
token_posn (LIT p _) = p 
}

