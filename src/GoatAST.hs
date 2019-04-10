module GoatAST where

type Ident = String

data BaseType 
  = BoolType | IntType 
    deriving (Show, Eq)

data Shape
  = ShapeVar
  | ShapeArr Int
  | ShapeMat Int Int
    deriving (Show, Eq)

data Idx
  = IdxVar
  | IdxArr Expr
  | IdxMat Expr Expr
    deriving (Show, Eq)

data Var
  = Var Ident Idx
    deriving (Show, Eq)

data Binop 
  = Op_add | Op_mul 
    deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | StrConst String
  | Id Ident
  | Add Expr Expr
  | Mul Expr Expr
  | UnaryMinus Expr
    deriving (Show, Eq)

data Stmt 
  = Assign Var Expr
  | Read Var
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data Decl 
  = Decl Ident BaseType Shape
    deriving (Show, Eq)

data Indi 
  = InVar | InRef
    deriving (Show, Eq)

data Para
  = Para Ident BaseType Indi
    deriving (Show, Eq)

data Proc
  = Proc Ident [Para] [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = Program [Proc]
    deriving (Show, Eq)
