{
module Main where
}

%wrapper "basic"

$digit       = 0-9
@alpha       = [a-zA-Z]
@digits      = $digit+
@stringlit   = \" [^\"]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n

rules :-

  $white+    ;    -- skip white space
  @comment   ;    -- skip comments
  @digits    { \s -> INT_CONST (read s :: Int) }
  true       { \s -> BOOL_CONST True }
  false      { \s -> BOOL_CONST False }
  bool       { \s -> BOOL }
  int        { \s -> INT }
  proc       { \s -> PROC }
  begin      { \s -> BEGIN }
  end        { \s -> END }
  read       { \s -> READ }
  write      { \s -> WRITE }
  :=         { \s -> ASSIGN }
  \(         { \s -> LPAREN }
  \)         { \s -> RPAREN }
  \+         { \s -> PLUS }
  \-         { \s -> MINUS }
  \*         { \s -> MUL }
  \;         { \s -> SEMI }
  @ident     { \s -> IDENT s }
  @stringlit { \s -> LIT s }

{
data Token
  = BOOL | INT | PROC | BEGIN | END | READ | WRITE | ASSIGN 
  | INT_CONST Int | BOOL_CONST Bool | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
    deriving (Eq, Show)

main
  = do
      s <- getContents
      print (alexScanTokens s)
}

