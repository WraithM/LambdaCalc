module Ast where

data Consts
    = Int
    | String
    | Float
    | Char

data Op
    = Add
    | Mult
    | Sub
    | Div
    | Concat

data Exp
    = Id String
    | Const Consts
    | Abs String Exp
    | Parens Exp
    | App Exp Exp
    | BinOp Op Exp Exp