-- Main.hs

-- module Main where

--   import Lib

--   main :: IO ()
--   main = putStrLn "Hello world!"

-- -- Testing stack project
-- -- [make sure you're in .../palindrome-checker directory]
-- -- $ stack test

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn,getLine)

main :: IO ()
main = do
  TIO.putStrLn "Enter some text and I'll determine if it's a palindrome:"
  text <- TIO.getLine
  let response = if isPalindrome text
                 then "palindrome"
                 else "not palindrome"
  TIO.putStrLn response
--
