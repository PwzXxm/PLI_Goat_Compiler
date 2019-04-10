module GoatAST where

type Ident = String

data BaseType 
  = BoolType | IntType | FloatType
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
  = Op_add
  | Op_sub
  | Op_mul 
  | Op_div
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
    deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Evar Var
  | Add Expr Expr -- Binary Operator
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Eq  Expr Expr
  | Ne  Expr Expr
  | Lt  Expr Expr
  | Le  Expr Expr
  | Gt  Expr Expr
  | Ge  Expr Expr
  | And Expr Expr
  | Or  Expr Expr
  | UnaryMinus Expr -- Unary operator
  | UnaryNot Expr
    deriving (Show, Eq)

data Stmt 
  = Assign Var Expr
  | Read Var
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt] [Stmt]
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
