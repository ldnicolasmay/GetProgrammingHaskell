-- unit6_lesson34.hs


-- head :: Monoid a => [a] -> a
-- head (x:_) = x
-- head []    = mempty

-- example :: [[Int]]
-- example = []

-- length :: Int
-- length = 8

-- doubleLength :: Int
-- doubleLength = Main.length * 2

module Main where

-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

-- import qualified Palindrome

import qualified Data.Text.IO as TIO (getLine)
import Palindrome

main :: IO ()
main = do
  putStrLn "Enter some text, and I'll determine if it's a palindrome: "
  -- text <- getLine     -- text :: String
  text <- TIO.getLine  -- text :: T.Text
  -- let response = if isPalindrome text
  --                then "It's a palindrome."
  --                else "It's not a palindrome."
  -- let response = if Palindrome.isPalindrome text
  --                then "It's a palindrome."
  --                else "It's not a palindrome."
  let response = if isPalindrome text
                 then "It's a palindrome."
                 else "It's not a palindrome."
  putStrLn response

