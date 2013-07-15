module Ast where

-- | Binary operators
data Op
    = Add
    | Mul
    | Sub
    | Div
    | Cat
    deriving (Show, Eq)

-- | An assignment between a name and an expression
type Assign = (String, Exp)

-- | Our expression grammar
data Exp
    = Id String
    | IntConst Integer
    | StrConst String
    | Abs String Exp
    | Parens Exp
    | App Exp Exp
    | BinOp Op Exp Exp
    deriving (Show, Eq)
