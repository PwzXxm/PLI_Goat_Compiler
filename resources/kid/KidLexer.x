{
module Main where
}

%wrapper "basic"

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
  @digits    { \s -> INT_CONST (read s :: Int) }
  @float     { \s -> FLOAT_CONST (read s :: Float) }
  true       { \s -> BOOL_CONST True }
  false      { \s -> BOOL_CONST False }
  bool       { \s -> BOOL }
  int        { \s -> INT }
  float      { \s -> FLOAT }
  proc       { \s -> PROC }
  begin      { \s -> BEGIN }
  end        { \s -> END }
  read       { \s -> READ }
  write      { \s -> WRITE }
  else       { \s -> ELSE }
  if         { \s -> IF }
  fi         { \s -> FI }
  do         { \s -> DO }
  od         { \s -> OD }
  val        { \s -> VAL }
  while      { \s -> WHILE}
  ref        { \s -> REF}
  then       { \s -> THEN}
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
  = BOOL | INT | FLOAT | PROC | BEGIN | END | READ | WRITE | ELSE | IF
  | FI | DO | OD | VAL | WHILE | REF | THEN | ASSIGN 
  | INT_CONST Int | BOOL_CONST Bool | FLOAT Float | IDENT String | LIT String
  | LPAREN | RPAREN | PLUS | MINUS | MUL | SEMI 
    deriving (Eq, Show)

main
  = do
      s <- getContents
      print (alexScanTokens s)
}

