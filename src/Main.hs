module Main where

import Control.Monad (forM_)
import Text.Parsec.String (parseFromFile)

import Parser
import Eval
import Ast

parseFile :: FilePath -> IO [Assign]
parseFile filename = do
    result <- parseFromFile parseAssignments filename
    case result of
        Left err -> do
            print err
            return []
        Right as -> return as

main :: IO ()
main = do
    as <- parseFile "test.l"
    forM_ as $ \a ->
        print a
