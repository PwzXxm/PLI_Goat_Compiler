module OzInstructionFormatter(instructionFormatter) where

import OzInstruction

-- | Print one instruction
instructionFormatter :: Instruction -> String
instructionFormatter x 
  = case x of
    (IComment _) -> "  " ++ instructionFormatter_ x
    (ILabel _)   -> instructionFormatter_ x
    _            -> "    " ++ instructionFormatter_ x


instructionFormatter_ :: Instruction -> String
instructionFormatter_ (IConstant c) = constantFormatter c
instructionFormatter_ (IOperation o) = operationFormatter o
instructionFormatter_ (IStatement s) = statementFormatter s
instructionFormatter_ (IDebug d) = debugFormatter d
instructionFormatter_ (IBranch b) = branchFormatter b
instructionFormatter_ (ICall l) = "call " ++ (lshow l)
instructionFormatter_ (ICall_bt bt) = "call_builtin " ++ bt
instructionFormatter_ (IPushStack i) = "push_stack_frame " ++ (show i)
instructionFormatter_ (IPopStack i) = "pop_stack_frame " ++ (show i)
instructionFormatter_ (IComment str) = "# " ++ str
instructionFormatter_ (IHalt) = "halt"
instructionFormatter_ (IReturn) = "return"
instructionFormatter_ (ILabel l) = (lshow l) ++ ":"

constantFormatter :: Constant -> String
constantFormatter (ConsInt r i) = "int_const " ++ (rshow r) ++ ", " ++ (show i)
constantFormatter (ConsFloat r f) = "real_const " ++ (rshow r) ++ ", " ++ (show f)
constantFormatter (ConsString r s) = "string_const " ++ (rshow r) ++ ", \"" ++ s ++ "\""


twoFormatter :: Reg -> Reg -> String
twoFormatter r1 r2 = (rshow r1) ++ ", " ++ (rshow r2)


threeFormatter :: Reg -> Reg -> Reg -> String
threeFormatter r1 r2 r3 = (rshow r1) ++ ", " ++ (rshow r2) ++ ", " ++ (rshow r3)

operationFormatter :: Operation -> String
operationFormatter (Add_off r1 r2 r3) = "add_offset " ++ (threeFormatter r1 r2 r3)
operationFormatter (Sub_off r1 r2 r3) = "sub_offset " ++ (threeFormatter r1 r2 r3)
operationFormatter (And_ r1 r2 r3) = "and " ++ (threeFormatter r1 r2 r3)
operationFormatter (Or_ r1 r2 r3) = "or " ++ (threeFormatter r1 r2 r3)
operationFormatter (Not_ r1 r2) = "not " ++ (twoFormatter r1 r2)
operationFormatter (Int2real r1 r2) = "int_to_real " ++ (twoFormatter r1 r2)
operationFormatter (Move r1 r2) = "move " ++ (twoFormatter r1 r2)
operationFormatter (Binary op t r1 r2 r3)
  = (binaryOpFormatter op)
    ++ "_" ++
    (typeFormatter t)
    ++ " " ++
    (threeFormatter r1 r2 r3)
operationFormatter (Unary op t r1 r2)
  = (unaryOpFormatter op)
    ++ "_" ++
    (typeFormatter t)
    ++ " " ++
    (twoFormatter r1 r2)

binaryOpFormatter :: BinaryOp -> String
binaryOpFormatter Add = "add"
binaryOpFormatter Sub = "sub"
binaryOpFormatter Mul = "mul"
binaryOpFormatter Div = "div"
binaryOpFormatter Eq = "cmp_eq"
binaryOpFormatter Ne = "cmp_ne"
binaryOpFormatter Gt = "cmp_gt"
binaryOpFormatter Ge = "cmp_ge"
binaryOpFormatter Lt = "cmp_lt"
binaryOpFormatter Le = "cmp_le"

unaryOpFormatter :: UnaryOp -> String
unaryOpFormatter NEG = "neg"


typeFormatter :: RegType -> String
typeFormatter INT = "int"
typeFormatter REAL = "real"

statementFormatter :: Statement -> String
statementFormatter (Store i r) = "store " ++ (show i) ++ ", " ++ (rshow r)
statementFormatter (Load r i) = "load " ++ (rshow r) ++ ", " ++ (show i)
statementFormatter (Load_ad r i) = "load_address " ++ (rshow r) ++ ", " ++ (show i)
statementFormatter (Load_in r1 r2) = "load_indirect " ++ (twoFormatter r1 r2)
statementFormatter (Store_in r1 r2) = "store_indirect " ++ (twoFormatter r1 r2)

rshow :: Reg -> String
rshow r = "r" ++ (show r)

lshow :: Label -> String
lshow l = "label_" ++ l

debugFormatter :: Debug -> String
debugFormatter (DebugReg r) = "debug_reg " ++ (rshow r)
debugFormatter (DebugSlot i) = "debug_slot " ++ (show i)
debugFormatter DebugStack = "debug_stack"

branchFormatter :: Branch -> String
branchFormatter (Cond True r l)
  = "branch_on_true " ++ (rshow r) ++ ", " ++ (lshow l)
branchFormatter (Cond False r l)
  = "branch_on_false " ++ (rshow r) ++ ", " ++ (lshow l)
branchFormatter (Uncond l)
  = "branch_uncond " ++ (lshow l)
