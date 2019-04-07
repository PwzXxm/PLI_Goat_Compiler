{
module GoatLexer where

import GoatToken

import Text.Parsec.Pos
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
  :=         { \p s -> (p, ASSIGN) }
  \(         { \p s -> (p, LPAREN) }
  \)         { \p s -> (p, RPAREN) }
  \+         { \p s -> (p, PLUS) }
  \-         { \p s -> (p, MINUS) }
  \*         { \p s -> (p, MUL) }
  \;         { \p s -> (p, SEMI) }
  @ident     { \p s -> (p, IDENT s) }
  @stringlit { \p s -> (p, LIT s) }

{
type AlexToken = (AlexPos, Tok)

mapFunc :: AlexToken -> Token
mapFunc ((AlexPn_ line col), tok) = ((SourcePos _ line col), tok)

runGoatLexer :: String -> [Token]
runGoatLexer s
  = do
    s <- getContents
    return map mapFunc (alexScanTokens s)
}