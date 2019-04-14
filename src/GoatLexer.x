{
module GoatLexer(runGoatLexer) where

import GoatToken

import Text.Parsec.Pos
}

%wrapper "posn"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
@float       = @digits \. @digits
@stringlit   = \" [^\"\n]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n

rules :-

  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { \p s -> (p, INT_CONST (read s :: Int)) }
  @float     { \p s -> (p, FLOAT_CONST (read s :: Float)) }
  true       { \p s -> (p, BOOL_CONST True) }
  false      { \p s -> (p, BOOL_CONST False) }
  bool       { \p s -> (p, BOOL) }
  int        { \p s -> (p, INT) }
  float      { \p s -> (p, FLOAT) }
  proc       { \p s -> (p, PROC) }
  begin      { \p s -> (p, BEGIN) }
  end        { \p s -> (p, END) }
  read       { \p s -> (p, READ) }
  write      { \p s -> (p, WRITE) }
  else       { \p s -> (p, ELSE) }
  if         { \p s -> (p, IF) }
  fi         { \p s -> (p, FI) }
  do         { \p s -> (p, DO) }
  od         { \p s -> (p, OD) }
  val        { \p s -> (p, VAL) }
  while      { \p s -> (p, WHILE) }
  ref        { \p s -> (p, REF) }
  then       { \p s -> (p, THEN) }
  call       { \p s -> (p, CALL)}
  :=         { \p s -> (p, ASSIGN) }
  \(         { \p s -> (p, LPAREN) }
  \)         { \p s -> (p, RPAREN) }
  \+         { \p s -> (p, PLUS) }
  \-         { \p s -> (p, MINUS) }
  \*         { \p s -> (p, MUL) }
  \/         { \p s -> (p, DIV) }
  \<         { \p s -> (p, LESS) }
  \<\=       { \p s -> (p, LESSEQUAL) }
  \>         { \p s -> (p, GREATER) }
  \>\=       { \p s -> (p, GREATEQUAL) }
  \=         { \p s -> (p, EQUAL) }
  \!\=       { \p s -> (p, UNEQUAL) }
  \[         { \p s -> (p, LSQUARE) }
  \]         { \p s -> (p, RSQUARE) }
  \|\|       { \p s -> (p, OR) }
  \&\&       { \p s -> (p, AND) }
  \!         { \p s -> (p, UNARYNOT) }
  \;         { \p s -> (p, SEMI) }
  \,         { \p s -> (p, COMMA) }
  @ident     { \p s -> (p, IDENT s) }
  @stringlit { \p s -> (p, LIT s) }

{
type AlexToken = (AlexPosn, Tok)

mapFunc :: String -> AlexToken -> Token
mapFunc filename ((AlexPn _ line col), tok) = ((newPos filename line col), (cleanStrLit tok))

-- remove start & end quote in string literal
cleanStrLit :: Tok -> Tok
cleanStrLit (LIT str) = (LIT (tail (init str)))
cleanStrLit tok = tok

runGoatLexer :: String -> String -> [Token]
runGoatLexer filename s 
  = map (mapFunc filename) (alexScanTokens s)
}