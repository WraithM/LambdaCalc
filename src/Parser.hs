module Parser where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

import Ast

-- Lexical values
lambda    = char '\\':: Parser Char
dot       = char '.' :: Parser Char
equals    = char '=' :: Parser Char
whiteChar = tab <|> space :: Parser Char

-- Parsers
endlines :: Parser ()
endlines = many1 newline >>= optional

whitespace :: Parser ()
whitespace = many1 whiteChar >>= optional

parseId :: Parser String
parseId = return (alpha >>= many1 alphaNum)

parseInt :: Parser Exp
parseInt = do
    s <- many1 digit
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
    c <- oneOf "+*-/@"
    whitespace
    e2 <- parseExp
    case lookup c opDict of
        Just op -> return $ BinOp op e1 e2
        Nothing -> error "Cannot parse BinOp"

parseParens :: Parser Exp
parseParens = do
    char '('
    e <- parseExp
    char ')'
    return e

parseStrConst :: Parser String
parseStrConst = do
    char '"'
    str <- many1 anyChar
    char '"'
    return (StrConst str)
    
parseLambda = do
    lambda
    whitespace
    x <- parseId
    whitespace
    dot
    whitespace
    e <- parseExp
    return (Abs x e)

parseApp = do
    l <- parseLambda
    whitespace
    a <- parseExp
    return (App l a)

parseAssign :: Parser Assign
parseAssign = do
    name <- parseId
    whitespace
    equals
    whitespace
    e <- parseExp
    endlines
    return (Assign name e)

parseExp :: Parser Exp
parseExp = parseId
    <|> parseLambda
    <|> parseInt
    <|> parseStrConst
    <|> parseParens
    <|> parseApp
    <|> parseOp