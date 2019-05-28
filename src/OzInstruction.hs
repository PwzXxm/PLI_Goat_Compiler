-- | Data structure for Oz instructions
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module OzInstruction where

type Reg = Int
type Label = String

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Eq
  | Ne
  | Gt
  | Ge
  | Lt
  | Le
    deriving (Show, Eq)

data UnaryOp
  = NEG
    deriving (Show, Eq)

data RegType
  = INT | REAL
  deriving (Show, Eq)

data Statement
  = Store Int Reg
  | Load Reg Int
  | Load_ad Reg Int
  | Load_in Reg Reg
  | Store_in Reg Reg
    deriving (Show, Eq)

data Constant
  = ConsInt Reg Int
  | ConsFloat Reg Float
  | ConsString Reg String
    deriving (Show, Eq)


data Operation
  = Binary BinaryOp RegType Reg Reg Reg
  | Unary UnaryOp RegType Reg Reg
  | Add_off Reg Reg Reg
  | Sub_off Reg Reg Reg
  | And_ Reg Reg Reg
  | Or_ Reg Reg Reg
  | Not_ Reg Reg
  | Int2real Reg Reg
  | Move Reg Reg
    deriving (Show, Eq)

data Instruction
  = IConstant Constant
  | IOperation Operation
  | IStatement Statement
  | IDebug Debug
  | ICall Label
  | ICall_bt String
  | IBranch Branch
  | IPushStack Int
  | IPopStack Int
  | IComment String
  | IHalt
  | IReturn
  | ILabel Label
    deriving (Show, Eq)

data Branch
  = Cond Bool Reg Label
  | Uncond Label
    deriving (Show, Eq)

data Debug
  = DebugReg Reg
  | DebugSlot Int
  | DebugStack
    deriving (Show, Eq)
