module Ast where

-- | Binary operators
data Op
    = Add
    | Mul
    | Sub
    | Div
    | Cat
    deriving Eq

instance Show Op where
    show Add = " + "
    show Mul = " * "
    show Sub = " - "
    show Div = " / "
    show Cat = " @ "

-- | An assignment between a name and an expression
type Assign = (String, Exp)

-- | Our expression grammar
data Exp
    = Var String
    | IntConst Integer
    | StrConst String
    | Abs String Exp
    | Parens Exp
    | App Exp Exp
    | BinOp Op Exp Exp
    deriving Eq

instance Show Exp where
    show (Parens e)     = "(" ++ show e ++ ")"
    show (Var s)        = s
    show (IntConst i)   = show i
    show (StrConst s)   = show s
    show (BinOp op a b) = show a ++ show op ++ show b
    show (Abs x e)      = "\\" ++ x ++ "." ++ show e
    show (App e1@(Abs _ _) e2) = show (Parens e1) ++ " " ++ show e2
    show (App e1 e2)    = show e1 ++ " " ++ show e2
