module Lexer where

-- import Data.List (foldl')
-- import Control.Applicative ((<$>),(<*>))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

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

lexer :: T.TokenParser ()
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

allOf p = do
    whitespace
    r <- p
    eof
    return r
