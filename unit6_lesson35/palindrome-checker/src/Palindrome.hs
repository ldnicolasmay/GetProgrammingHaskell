-- Palindrome.hs

-- {-# LANGUAGE OverloadedStrings #-}

module Palindrome
  ( isPalindrome
  ) where

import qualified Data.Text as T (Text,filter,toLower,reverse)
import Data.Char (isSpace,isPunctuation)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
--