-- | Main module of this code generater
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module OzInstruction where

type Reg = Int

data BinaryOp = ADD | SUB | MUL | DIV | NEG

data CmpOp = EQ | NE | GT | GE | LT | LE

data Statement
  = Store Int Reg
  | Load Reg Int
  | Load_ad Reg Int
  | Load_in Reg Reg
  | Store_in Reg Reg

data Constant
  = ConsInt Reg Int
  | ConsFloat Float
  | ConsString String


data Operation
  = BinaryOp3 Reg Reg Reg
  | BinaryOp2 Reg Reg
  | CmpOp Reg Reg Reg 
  | Add_of Reg Reg Reg
  | Sub_off Reg Reg Reg
  | And_ Reg Reg Reg
  | Or_ Reg Reg Reg
  | Not_ Reg Reg
  | Int2real Reg Reg
  | Move Reg Reg Reg

data Instruction
  = Constant
  | Operation
  | Statement
  | Debug
  | Call Label
  | Call_bt String
  | If_else Branch Branch Branch
  | If Branch Branch
  | While Branch Branch Branch
  | Push Int
  | Pop Int
  | Comment String

data Label
  = Label Int [Instruction]

data Branch
  = Cond Bool Reg Label
  | Uncond Label

data Prog
  = Int [Statement]

data Debug
  = DebugReg Reg
  | DebugSlot Int
  | DebugStack


