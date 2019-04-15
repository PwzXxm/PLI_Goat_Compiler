-- | Data structure for the Abstract Syntax Tree
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatAST where

type Ident = String

data BaseType
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

-- | for variable declaration
data Shape
  = ShapeVar
  | ShapeArr Int
  | ShapeMat Int Int
    deriving (Show, Eq)

-- | for variable index
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
  | BinaryOp Binop Expr Expr -- Binary Operator
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
