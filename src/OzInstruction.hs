-- | Main module of this code generater
--
-- Authors:
--   Weizhi Xu  (752454)
--   Zijun Chen (813190)
--   Zhe Tang   (743398)

module OzInstruction where

type Reg = Int
type Label = String

data BinaryOp
  = ADD | SUB | MUL | DIV | NEG
    deriving (Show, Eq)

data RegType
  = INT | REAL
  deriving (Show, Eq)

data CmpOp
  = Eq | Ne | Gt | Ge | Lt | Le
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
  = Op3 BinaryOp RegType Reg Reg Reg
  | Op2 BinaryOp RegType Reg Reg
  | Cmp CmpOp RegType Reg Reg Reg 
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


-- -- Writer used for IO like output with higher performance than (++)
-- -- From https://kseo.github.io/posts/2017-01-21-writer-monad.html
-- import           Control.Monad.Trans.Writer.Strict
-- import           Data.Monoid

-- output :: String -> Writer (Endo [String]) ()
-- output x = tell $ Endo ([x]<>)

-- type StrWriter = Writer (Endo [String]) ()
-- ----------------------------------------------

-- -- | Concatenate strings into one
-- concatenateString :: [Instruction] -> String
-- concatenateString instructions
--   = concat (appEndo (execWriter (instructionsFormatter instructions)) [])

-- -- | Print the output
-- instructionOutput :: [Instruction] -> IO ()
-- instructionOutput instructions
--   = mapM_ putStr (appEndo (execWriter (instructionsFormatter instructions)) [])

-- -- | Print instructions in turn
-- instructionsFormatter :: [Instruction] -> StrWriter
-- instructionsFormatter [x] = instructionFormatter x
-- instructionsFormatter (x:xs)
--   = do
--       instructionFormatter x
--       output "\n"
--       instructionsFormatter xs

-- | Print one instruction
instructionFormatter :: Instruction -> String
instructionFormatter (IConstant c) = constantFormatter c
instructionFormatter (IOperation o) = operationFormatter o
instructionFormatter (IStatement s) = statementFormatter s
instructionFormatter (IDebug d) = debugFormatter d
instructionFormatter (IBranch b) = branchFormatter b
instructionFormatter (ICall l) = "call " ++ (lshow l)
instructionFormatter (ICall_bt bt) = "call_builtin " ++ bt
instructionFormatter (IPushStack i) = "push_stack_frame " ++ (show i)
instructionFormatter (IPopStack i) = "pop_stack_frame " ++ (show i)
instructionFormatter (IComment str) = "# " ++ str


constantFormatter :: Constant -> String
constantFormatter (ConsInt r i) = "int_const " ++ (rshow r) ++ ", " ++ (show i)
constantFormatter (ConsFloat r f) = "real_const " ++ (rshow r) ++ ", " ++ (show f)
constantFormatter (ConsString r s) = "string_const " ++ (rshow r) ++ ", " ++ (show s)


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
operationFormatter (Op3 op t r1 r2 r3)
  = (binaryOpFormatter op)
    ++ "_" ++
    (typeFormatter t)
    ++ " " ++
    (threeFormatter r1 r2 r3)
operationFormatter (Op2 op t r1 r2)
  = (binaryOpFormatter op)
    ++ "_" ++
    (typeFormatter t)
    ++ " " ++
    (twoFormatter r1 r2)
operationFormatter (Cmp op t r1 r2 r3)
  = "cmp_" ++ (cmpFormatter op)
    ++ "_" ++
    (typeFormatter t)
    ++ " " ++
    (threeFormatter r1 r2 r3)


binaryOpFormatter :: BinaryOp -> String
binaryOpFormatter ADD = "add"
binaryOpFormatter SUB = "sub"
binaryOpFormatter MUL = "mul"
binaryOpFormatter DIV = "div"
binaryOpFormatter NEG = "neg"

typeFormatter :: RegType -> String
typeFormatter INT = "int"
typeFormatter REAL = "real"

statementFormatter :: Statement -> String
statementFormatter (Store i r) = "store " ++ (show i) ++ ", " ++ (rshow r)
statementFormatter (Load r i) = "load " ++ (rshow r) ++ ", " ++ (show i)
statementFormatter (Load_ad r i) = "load_address" ++ (rshow r) ++ ", " ++ (show i)
statementFormatter (Load_in r1 r2) = "load_indirect" ++ (twoFormatter r1 r2)
statementFormatter (Store_in r1 r2) = "store_indirect" ++ (twoFormatter r1 r2)

rshow :: Reg -> String
rshow r = "r" ++ (show r)

lshow :: Label -> String
lshow l = "label_" ++ (show l)

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

cmpFormatter :: CmpOp -> String
cmpFormatter Eq = "eq"
cmpFormatter Ne = "ne"
cmpFormatter Gt = "gt"
cmpFormatter Ge = "ge"
cmpFormatter Lt = "lt"
cmpFormatter Le = "le"
