-- | Data structure for the Abstract Syntax Tree
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module GoatAST where

import           Text.Parsec.Pos
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
  = Var SourcePos Ident Idx
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
  = BoolConst SourcePos Bool
  | IntConst SourcePos Int
  | FloatConst SourcePos Float
  | StrConst SourcePos String
  | Evar SourcePos Var
  | BinaryOp SourcePos Binop Expr Expr -- Binary Operator
  | UnaryMinus SourcePos Expr -- Unary operator
  | UnaryNot SourcePos Expr
    deriving (Show, Eq)

data Stmt
  = Assign SourcePos Var Expr
  | Read SourcePos Var
  | Write SourcePos Expr
  | Call SourcePos Ident [Expr]
  | If SourcePos Expr [Stmt] [Stmt]
  | While SourcePos Expr [Stmt]
    deriving (Show, Eq)

data Decl
  = Decl SourcePos Ident BaseType Shape
    deriving (Show, Eq)

data Indi
  = InVal | InRef
    deriving (Show, Eq)

data Para
  = Para SourcePos Ident BaseType Indi
    deriving (Show, Eq)

data Proc
  = Proc SourcePos Ident [Para] [Decl] [Stmt]
    deriving (Show, Eq)

data GoatProgram
  = Program [Proc]
    deriving (Show, Eq)


--------------------------------------------------
--  Decorated Asbtract Syntax Tree
--------------------------------------------------
type SlotNum = Int
type SlotSize = Int
type ProcId = Int

-- | for variable index
data DIdx
  = DIdxVar
  | DIdxArr DExpr
  | DIdxMat DExpr DExpr
    deriving (Show, Eq)

data DVar
  = DVar SlotNum Shape BaseType
    deriving (Show, Eq)

data DExpr
  = DBoolConst Bool
  | DIntConst Int
  | DFloatConst Float
  | DStrConst String
  | DEvar DVar
  | DBinaryOp Binop DExpr DExpr BaseType -- Binary Operator
  | DUnaryMinus DExpr BaseType -- Unary operator
  | DUnaryNot DExpr BaseType
    deriving (Show, Eq)

data DStmt
  = DAssign DVar DExpr
  | DRead DVar
  | DWrite DExpr
  | DCall ProcId [DExpr]
  | DIf DExpr [DStmt] [DStmt]
  | DWhile DExpr [DStmt]
    deriving (Show, Eq)

data DPara
  = DPara SlotNum Indi BaseType
    deriving (Show, Eq)

data DProc
  = DProc ProcId [DPara] [DStmt] SlotSize
    deriving (Show, Eq)

data DGoatProgram
  = DProgram ProcId [DProc]
    deriving (Show, Eq)

data DProcProto
  = DProcProto ProcId [DProcProtoPara]

data DProcProtoPara
  = DProcProtoPara Indi BaseType
    deriving (Show, Eq)
