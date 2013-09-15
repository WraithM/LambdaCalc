module Parser where

import Data.List (foldl')
import Control.Applicative ((<$>),(<*>))

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
    , T.opLetter        = oneOf "=+*-/@\\"
    }

lexer = T.makeTokenParser lambdaDef

parens     = T.parens lexer
identifier = T.identifier lexer
reserved   = T.reserved lexer
integer    = T.integer lexer
strLiteral = T.stringLiteral lexer
operator   = T.operator lexer
reservedOp = T.reservedOp lexer
whitespace = T.whiteSpace lexer
dot        = T.dot lexer

lambda = reservedOp "\\"
equals = reservedOp "="

parseVar :: Parser Exp
parseVar = do
    x <- identifier
    return (Var x)

parseInt :: Parser Exp
parseInt = do
    i <- integer
    return $ IntConst i

opDict :: [(String, Op)]
opDict =
    [ ("+", Add)
    , ("*", Mul)
    , ("-", Sub)
    , ("/", Div)
    , ("@", Cat)
    ]
    
-- Still having some trouble
parseOp :: Parser Exp
parseOp = do
    e1 <- parseExp
    opName <- operator
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
    
-- Problem exists here: spaceSep includes \n as a space
spaceSep :: Parser a -> Parser [a]
spaceSep p = (:) <$> p <*> many (try (spaces' >> p))
  where spaces' = skipMany (skipMany1 (satisfy isSpace))
        isSpace c = c == ' ' || c == '\t'

makeLambda :: [String] -> Exp -> Exp
makeLambda xs e = foldl' (flip Abs) e (reverse xs)

parseLambda :: Parser Exp
parseLambda = do
    lambda
    xs <- spaceSep identifier
    dot
    e <- parseExp
    return $ makeLambda xs e
    <?> "lambda"
    
parseApp :: Parser Exp
parseApp = do
    tlist <- spaceSep term
    return $ case tlist of
        [t] -> t
        t:ts -> foldl' App t ts
    <?> "application"
  where
    term = choice [ parseInt, parseStrConst ] <|> (parseVar <|> parseLambda <|> parens term)

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
parseAssignments = parseAssign `sepEndBy` whitespace
    
parseExp :: Parser Exp
parseExp = try parseApp
    <|> try parseLambda
    <|> try (parens parseExp)
    <|> parseOp
    <|> parseVar
    <|> parseInt
    <|> parseStrConst
    <?> "exp"
