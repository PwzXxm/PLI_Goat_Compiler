-- | Main module of this code generater
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module CodeGenerator where

data Register = Tint Int | Treal Int

data BinaryOp = ADD | SUB | MUL | DIV | NEG

data CmpOp = EQ | NE | GT | GE | LT | LE

data Instruction
  = Store Int Register
  | Load Register Int
  | Load_ad Register Int
  | Load_in Register Register
  | Store_in Register Register

data Constant
  = Register Int
  | Register Float
  | Register String


data Operation
  = BinaryOp Register Register Register
  | BinaryOp Register Register
  | CmpOp Register Register Register 
  | Add_of Register Register Register
  | Sub_off Register Register Register
  | And Register Register Register
  | Or Register Register Register
  | Not Register Register
  | Int2real Register Register
  | Move Register Register Register

data Statement
  = Constant
  | Operation
  | Instruction
  | Call Label
  | Call_bt String
  | If Branch Branch Branch
  | If Branch Branch
  | While Branch Branch Branch


data Label
  = Int [Expression]

data Branch
  = True Register Label
  = False Register Label
  = Uncond Label

data Prog
  = Int [Statement]

data Debug
  = Reg Register
  = Slot Int
  = Stack 