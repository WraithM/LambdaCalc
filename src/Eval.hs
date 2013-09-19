module Eval where

import Data.Maybe (fromMaybe)

import Ast

-- | Apply a binary operation after evaluating two expressions
applyOp :: Op -> Exp -> Exp -> Exp
applyOp Add e1 e2 = IntConst (i1 + i2)
  where
    IntConst i1 = eval e1
    IntConst i2 = eval e2
applyOp Mul e1 e2 = IntConst (i1 * i2)
  where
    IntConst i1 = eval e1
    IntConst i2 = eval e2
applyOp Sub e1 e2 = IntConst (i1 - i2)
  where
    IntConst i1 = eval e1
    IntConst i2 = eval e2
applyOp Div e1 e2
    | i2 == 0 = error "Div by Zero"
    | otherwise = IntConst (i1 `div` i2)
  where
    IntConst i1 = eval e1
    IntConst i2 = eval e2
applyOp Cat e1 e2 = StrConst (s1 ++ s2)
  where
    StrConst s1 = eval e1
    StrConst s2 = eval e2

-- | Evaluate an expression via substitution
eval :: Exp -> Exp
eval (BinOp op e1 e2) = applyOp op e1 e2
eval (Var x) = Var x
eval (IntConst i) = IntConst i
eval (StrConst s) = StrConst s
eval (Parens e) = eval e
eval (Abs x e) = Abs x e
eval (App (Abs x e) arg) = eval (substitute arg x e)
eval e = error ("Eval: Cannot eval " ++ show e)

-- | Substitute an expression for a variable name in an expression
substitute :: Exp -> String -> Exp -> Exp
substitute arg x (Var y) = if x == y then arg else Var y
substitute arg x (BinOp op e1 e2) = BinOp op (substitute arg x e1) (substitute arg x e2)
substitute arg x (Parens e) = substitute arg x e
substitute arg x (Abs y e) 
    | x == y = Abs y e
    | otherwise = Abs y (substitute arg x e)
substitute arg x (App e1 e2) = App (substitute arg x e1) (substitute arg x e2)
substitute _ _ e = e

-- | Get a list of variable names in an expression. I should get only free variables.
getVars :: Exp -> [String]
getVars (Parens e) = getVars e
getVars (Var x) = [x]
getVars (BinOp _ e1 e2) = getVars e1 ++ getVars e2
getVars (IntConst _) = []
getVars (StrConst _) = []
getVars (Abs _ e) = getVars e
getVars (App e1 e2) = getVars e1 ++ getVars e2

-- | Given a set of assignments, evaluate the expression named "main"
evalMain :: [Assign] -> Exp
evalMain as = eval mainWithSubs
  where
    lookupId name = fromMaybe (Var name) (lookup name as)
    mainExp = lookupId "main"
    varsInMain = getVars mainExp
    mainWithSubs = foldl (\e n -> substitute (lookupId n) n e) mainExp varsInMain
