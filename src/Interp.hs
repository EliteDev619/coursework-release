module Interp (eval) where
import Ast
import Instruction

-- An environment maps variables to values. This allows you to look up
-- what a variable stands for when you use it (for example, after let-binding,
-- or using a parameter)
type Env = [(Variable, Value)]

-- TurtleState is a Haskell record which contains the state you will need when
-- evaluating. It contains the environment, the definition map, and the list of
-- instructions that you are building up.
data TurtleState = TS {
    env :: Env,
    definitions :: [(DefName, Definition)],
    instructions :: [Instruction]
}

-- Constructs an empty state, given a definition map
emptyState :: [(DefName, Definition)] -> TurtleState
emptyState defs = TS { env = [], definitions = defs, instructions = [] }

{- Exercise 2 -}
evalUnOp :: UnaryOp -> Value -> Value
evalUnOp _op _val = error "fill me in"

evalBinOp :: BinaryOp -> Value -> Value -> Value
evalBinOp Add (VInt i1) (VInt i2) = VInt (i1 + i2)
evalBinOp _op _v1 _v2 = error "fill me in"

{- Exercise 3 -}
addInstruction :: TurtleState -> Instruction -> TurtleState
addInstruction _st _i = error "fill me in"

bind :: TurtleState -> Variable -> Value -> TurtleState
bind st _var _val = error "fill me in"

lookupVar :: TurtleState -> Variable -> Value
lookupVar _st _var = error "fill me in"

lookupDef :: TurtleState -> DefName -> Definition
lookupDef _st _defName = error "fill me in"

{- Exercise 4 -}
evalExpr :: TurtleState -> Expr -> (Value, TurtleState)
evalExpr st (EInt i) = (VInt i, st)
evalExpr _st _e = error "fill me in"

-- External function
eval :: Program -> (Value, [Instruction])
eval (_defs, _e) = error "fill me in"
