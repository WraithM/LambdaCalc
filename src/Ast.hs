module Ast where

data Op
    = Add
    | Mul
    | Sub
    | Div
    | Cat
    deriving (Show, Eq)

type Assign = (String, Exp)

data Exp
    = Id String
    | IntConst Int
    | StrConst String
    | Abs String Exp
    | Parens Exp
    | App Exp Exp
    | BinOp Op Exp Exp
    deriving (Show, Eq)
