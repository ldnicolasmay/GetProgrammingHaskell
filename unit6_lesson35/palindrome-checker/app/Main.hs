-- Main.hs

-- {-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (putStrLn,getLine)

main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll determing if it's a palindrome:"
  text <- TIO.getLine
  let response = if isPalindrome text
                 then "palindrome"
                 else "not palindrome"
  TIO.putStrLn response
--

-- Building the package
-- $ stack setup
-- $ stack build

-- Running the executable in the package
-- $ stack exec palindrome-checker-exe
-- $ stack --silent exec palindrome-checker-exe