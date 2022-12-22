module Ast where

import Graphics.Gloss.Data.Color

{- Type synonyms -}
-- Definition names
type DefName = String
-- Parameter names
type Parameter = String
-- Definitions: lists of parameter, type pairs; return type; body
type Definition = ([(Parameter, Type)], Type, Expr)
-- Distance (type synonym for integer)
type Distance = Int
-- Variable name
type Variable = String

-- Program: map between definition names and definitions; body expression
type Program = ([(DefName, Definition)], Expr)

{- AST -}

-- Base types: int, bool, ()
data Type = TyInt | TyBool | TyUnit
    deriving (Eq, Show)

-- Direction
data Direction = Forward | Backward
    deriving (Eq, Show)

-- Rotation direction
data RotateDirection = RotateLeft | RotateRight
    deriving (Eq, Show)

-- The usual binary and unary operators
data BinaryOp =
      Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Greater
    | Less
    | GreaterEq
    | LessEq
    | And
    | Or
    deriving (Eq, Show)

data UnaryOp = Neg | Not
    deriving (Eq, Show)

-- Values are expressions that cannot reduce any further.
-- Evaluating an expression will produce a value; these
-- are not found in the parsed source.
data Value =
      VUnit
    | VInt Int
    | VBool Bool
    deriving (Eq, Show)

-- Values can be compared (but comparison is only defined on
-- values of the same type).
instance Ord Value where
    (<=) VUnit VUnit = True
    (<=) (VInt i1) (VInt i2) = i1 <= i2
    (<=) (VBool b1) (VBool b2) = b1 <= b2
    (<=) _ _ = undefined

-- Expressions.
data Expr =
    {- Data -}
      EVar Variable -- Variable names
    | EInt Int -- Integer literals
    | EBool Bool -- Boolean literals
    | EUnit -- Unit ()
    {- Operations -}
    | EBinOp BinaryOp Expr Expr -- Application of a binary operator
    | EUnOp UnaryOp Expr -- Application of a unary operator
    -- Function application (e.g., add(5 + 10, 10) = EApp "add" [(EBinOp Add [(EInt 5), (EInt 10)]), EInt 10])
    | EApp DefName [Expr] -- ^
    | EIf Expr Expr Expr -- Conditionals
    {- Actions -}
    | EChangeColor Color -- e.g., changeColor(red)
    | EMove Direction Expr -- e.g., forward(100) = EMove Forward 100
    | ERotate RotateDirection Expr -- e.g., left(90) = ERotate RotateLeft 90
    -- penUp and penDown
    | EPenUp
    | EPenDown
    {- Binding and Sequencing -}
    -- Let binding: let x = 5 in x  = ELet "x" (EInt 5) (EVar "x")
    | ELet Variable Expr Expr
    -- Sequencing: e1; e2 = ESeq e1 e2
    | ESeq Expr Expr
    deriving (Eq, Show)
