module Eval where

import Ast

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

eval :: Exp -> Exp
eval (BinOp op e1 e2) = applyOp op e1 e2
eval (Id x) = Id x
eval (IntConst i) = IntConst i
eval (StrConst s) = StrConst s
eval (Abs x e) = Abs x e
eval (App (Abs x e) arg) = eval (substitute arg x e)
eval (Parens e) = eval e
eval e = error ("Eval: Cannot eval " ++ show e)

substitute :: Exp -> String -> Exp -> Exp
substitute arg x (BinOp op e1 e2) = BinOp op (substitute arg x e1) (substitute arg x e2)
substitute arg x (Id y) = if x == y then arg else Id y
substitute arg x (Abs y e)
    | x == y = error $ "Cannot substitute: " ++ show x ++ " to " ++ show y
    | otherwise = Abs y (substitute arg x e)
substitute arg x (App e1 e2) = App (substitute arg x e1) (substitute arg x e2)
substitute arg x (Parens e) = Parens (substitute arg x e)
substitute _ _ e = e

getIds :: Exp -> [String]
getIds (Id x) = [x]
getIds (BinOp _ e1 e2) = getIds e1 ++ getIds e2
getIds (IntConst _) = []
getIds (StrConst _) = []
getIds (Abs _ e) = getIds e
getIds (App e1 e2) = getIds e1 ++ getIds e2
getIds (Parens e) = getIds e

evalMain :: [Assign] -> Exp
evalMain as = eval mainWithSubs
  where
    lookupId name = case lookup name as of
        Just e -> e
        Nothing -> Id name
    mainExp = lookupId "main"
    idsInMain = getIds mainExp
    mainWithSubs = foldl (\e n -> substitute (lookupId n) n e) mainExp idsInMain
