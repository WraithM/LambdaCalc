module Parser where

import qualified Data.HashMap.Strict as H
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import Ast

lambdaDef = emptyDef
    { commentStart   = "{-"
    , commentEnd     = "-}"
    , commentLine    = "--"
    , nestedComments = True
    , identStart     = letter
    , identLeter     = alphaNum <|> oneOf "_'"
    , opStart        = opLetter lambdaDef
    , opLetter       = oneOf "=+*-/@"
    , reservedNames  = ["\\","="]
    }

lexer = T.makeTokenParser lambdaDef

parens     = T.parens lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
integer    = T.integer lexer
strLiteral = T.stringLiteral lexer
operater   = T.operator lexer
whitespace = T.whiteSpace lexer
dot        = T.dot lexer

lambda = reserved "\\"
equal  = reserved "="

parseExpId :: Parser Exp
parseExpId = do
    x <- identifier
    return (Id x)

parseInt :: Parser Exp
parseInt = do
    s <- integer
    return $ IntConst (read s)

opDict :: [(Char, Op)]
opDict =
    [ ('+', Add)
    , ('*', Mul)
    , ('-', Sub)
    , ('/', Div)
    , ('@', Cat)
    ]
    
parseOp :: Parser Exp
parseOp = do
    e1 <- parseExp
    whitespace
    op <- operator
    whitespace
    e2 <- parseExp
    case lookup op opDict of
        Just r -> return $ BinOp r e1 e2
        Nothing -> error "Cannot parse BinOp"
    <?> "binop"

parseStrConst :: Parser Exp
parseStrConst = do
    str <- strLiteral
    return (StrConst str)
    <?> "strconst"
    
parseLambda :: Parser Exp
parseLambda = do
    lambda
    x <- identifier
    dot
    whitespace
    e <- parseExp
    return (Abs x e)
    <?> "lambda"

parseApp :: Parser Exp
parseApp = do
    l <- parseLambda
    whitespace
    a <- parseExp
    return (App l a)
    <?> "application"

parseAssign :: Parser Assign
parseAssign = do
    name <- identifier
    equals
    e <- parseExp
    return (name, e)
    <?> "assignment"
    
parseAssignments :: Parser [Assign]
parseAssignments = endBy parseAssign newline <?> "assignments"
    
parseExp :: Parser Exp
parseExp = parseLambda
    <|> parseApp
    <|> parseInt
    <|> parseStrConst
    <|> parens (parseExp) 
    <|> parseOp
    <|> parseExpId
    <?> "exp"
