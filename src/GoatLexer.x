{
module Main where
}

%wrapper "basic"

rules :-
    .*  { \s -> ANY }

{
data Token
  = ANY
  deriving (Eq, Show)

main
  = do
      s <- getContents
      print (alexScanTokens s)
}

