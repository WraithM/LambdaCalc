module Main where

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = putStrLn $ show fibs