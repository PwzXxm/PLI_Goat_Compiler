module GoatAST where

type Ident = String

data BaseType 
  = BoolType | IntType 
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
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
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data Decl 
  = Decl Ident BaseType
    deriving (Show, Eq)

data Indi 
  = Var | Ref
    deriving (Show, Eq)

data Para
  = Para Indi Decl
    deriving (Show, Eq)

data Proc
  = Proc Ident [Para] [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = Program [Proc]
    deriving (Show, Eq)
