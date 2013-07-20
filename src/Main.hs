module Main where

import System.Environment (getArgs)

import Text.Parsec (parse, newline)
import Text.Parsec.Combinator
import Text.Parsec.String (parseFromFile)

import Ast
import Eval
import Parser

-- | Parse a file with assignments at the top-level
parseFile :: FilePath -> IO [Assign]
parseFile filename = do
    result <- parseFromFile parseAssignments filename
    case result of
        Left err -> do
            print err
            return []
        Right as -> return as

getFirstArg :: IO String
getFirstArg = do
    args <- getArgs
    case args of
        [] -> error "No first argument."
        x:_ -> return x

main :: IO ()
main = getFirstArg >>= parseFile >>= mapM_ print
