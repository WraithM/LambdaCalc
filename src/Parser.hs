module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

import Ast

lambdaDef = emptyDef
    { T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.commentLine     = "--"
    , T.nestedComments  = True
    , T.identStart      = letter
    , T.identLetter     = alphaNum <|> oneOf "_'"
    , T.opStart         = T.opLetter lambdaDef
    , T.opLetter        = oneOf "=+*-/@"
    , T.reservedNames   = ["\\","="]
    }

lexer = T.makeTokenParser lambdaDef

parens     = T.parens lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
integer    = T.integer lexer
strLiteral = T.stringLiteral lexer
operator   = T.operator lexer
whitespace = T.whiteSpace lexer
dot        = T.dot lexer

lambda = reserved "\\"
equals = reserved "="

parseExpId :: Parser Exp
parseExpId = do
    x <- identifier
    return (Id x)

parseInt :: Parser Exp
parseInt = do
    i <- integer
    return $ IntConst i

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
    opName <- operator
    whitespace
    e2 <- parseExp
    case lookup opName opDict of
        Just op -> return $ BinOp op e1 e2
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
    whitespace
    x <- identifier
    whitespace
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
    whitespace
    equals
    whitespace
    e <- parseExp
    return (name, e)
    <?> "assignment"
    
parseAssignments :: Parser [Assign]
parseAssignments = endBy parseAssign newline <?> "assignments"
    
parseExp :: Parser Exp
parseExp = parseLambda
    <|> parens parseExp
    <|> parseApp
    <|> parseInt
    <|> parseStrConst
    <|> parseOp
    <|> parseExpId
    <?> "exp"
