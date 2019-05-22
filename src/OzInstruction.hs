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
  = Reg Int
  | Reg Float
  | Reg String


data Operation
  = BinaryOp Reg Reg Reg
  | BinaryOp Reg Reg
  | CmpOp Reg Reg Reg 
  | Add_of Reg Reg Reg
  | Sub_off Reg Reg Reg
  | And Reg Reg Reg
  | Or Reg Reg Reg
  | Not Reg Reg
  | Int2real Reg Reg
  | Move Reg Reg Reg

data Instruction
  = Constant
  | Operation
  | Statement
  | Debug
  | Call Label
  | Call_bt String
  | If Branch Branch Branch
  | If Branch Branch
  | While Branch Branch Branch
  | Push Int
  | Pop Int
  | Comment String

data Label
  = Int [Expression]

data Branch
  = Cond Bool Reg Label
  | Uncond Label

data Prog
  = Int [Statement]

data Debug
  = DebugReg Reg
  | DebugSlot Int
  | DebugStack 