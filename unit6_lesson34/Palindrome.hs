-- Palindrome.hs

module Palindrome (isPalindrome) where

-- import Data.Char (toLower,isSpace,isPunctuation)
import Data.Char (isSpace,isPunctuation)
import qualified Data.Text as T (Text,filter,toLower,reverse)

-- stripWhiteSpace :: String -> String
-- stripWhiteSpace = filter (not . isSpace)
stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

-- stripPunctuation :: String -> String
-- stripPunctuation = filter (not . isPunctuation)
stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

-- toLowerCase :: String -> String
-- toLowerCase = map toLower
-- toLowerCase :: T.Text -> T.Text
-- toLowerCase = T.toLower

-- preprocess :: String -> String
-- preprocess = stripWhiteSpace . stripPunctuation . toLowerCase
preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

-- isPalindrome :: String -> Bool
-- isPalindrome text = cleanText == reverse cleanText
--   where cleanText = preprocess text
isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
