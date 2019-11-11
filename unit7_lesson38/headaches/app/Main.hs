module Main where

import Lib

main :: IO ()
-- main = someFunc
main = do
  putStrLn "Enter a number to test for primality:"
  -- n <- read <$> getLine
  input <- getLine
  let n = read input :: Int
  let result = isPrime n
  putStrLn (displayResult result)

