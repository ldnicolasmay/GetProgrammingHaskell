-- Main.hs

-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Palindrome
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn,getLine)

main :: IO ()
main = do
  TIO.putStrLn "Enter some text and I'll determine if it's a palindrome:"
  text <- TIO.getLine
  let response = if Palindrome.isPalindrome text
                 then "palindrome"
                 else "not palindrome"
  TIO.putStrLn response
  Lib.someFunc
--

-- Building the package
-- [make sure you're in .../palindrome-checker directory]
-- $ stack setup
-- $ stack build

-- Running the executable in the package
-- [make sure you're in .../palindrome-checker directory]
-- $ stack exec palindrome-checker-exe
-- $ stack --silent exec palindrome-checker-exe