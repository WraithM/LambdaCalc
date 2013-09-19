module Parser where

import Data.List (foldl')
import Control.Applicative ((<$>))

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as E

import Lexer
import Ast

pVar :: Parser Exp
pVar = Var <$> identifier <?> "Var"

pInt :: Parser Exp
pInt = IntConst <$> integer <?> "Int"
   
pStrConst :: Parser Exp
pStrConst = StrConst <$> strLiteral
    <?> "strconst"
    
makeLambda :: [String] -> Exp -> Exp
makeLambda xs e = foldl' (flip Abs) e (reverse xs)

pLambda :: Parser Exp
pLambda = do
    lambda
    xs <- many1 identifier
    dot
    e <- pExp
    return $ makeLambda xs e
    <?> "lambda"
    
pFactor :: Parser Exp
pFactor = Parens <$> parens pExp
    <|> pVar
    <|> pInt
    <|> pStrConst
    <|> pLambda
    <?> "factor"

opDict :: [(String, Op)]
opDict =
    [ ("+", Add)
    , ("*", Mul)
    , ("-", Sub)
    , ("/", Div)
    , ("@", Cat)
    ]
 
pTerm :: Parser Exp
pTerm = E.buildExpressionParser table pFactor <?> "term"
  where infixOp x = E.Infix (reservedOp x >> return (\e1 e2 -> 
            case lookup x opDict of
                Nothing -> error "Can't find operator"
                Just op -> BinOp op e1 e2))
        table =
            [ [ infixOp "*" E.AssocLeft ]
            , [ infixOp "/" E.AssocNone ]
            , [ infixOp "+" E.AssocLeft, infixOp "@" E.AssocLeft ]
            , [ infixOp "-" E.AssocNone ]
            ]

pExp :: Parser Exp
pExp = foldl1 App <$> many1 pTerm
    <?> "exp"

pAssign :: Parser Assign
pAssign = do
    name <- identifier
    equals
    e <- pExp
    return (name, e)
    <?> "assignment"

pAssignments :: Parser [Assign]
pAssignments = pAssign `sepEndBy` semi
    <?> "assignments"

parseExp t = case parse (allOf pAssignments) "" t of
    Left err -> error $ show err
    Right asgn -> asgn
