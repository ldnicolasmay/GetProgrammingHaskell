-- Lib.hs

-- module Lib
--   ( isPalindrome
--   , preprocess
--   ) where

-- import Data.Text as T (Text,filter,reverse)
-- import Data.Char (isPunctuation)

-- -- preprocess :: String -> String
-- -- -- preprocess = filter (not . (`elem` ['!','.','#',')']))
-- -- preprocess = filter (not . isPunctuation)
-- preprocess :: T.Text -> T.Text
-- preprocess = T.filter (not . isPunctuation)


-- -- isPalindrome :: String -> Bool
-- -- -- isPalindrome text = text == reverse text
-- -- isPalindrome text = cleanText == reverse cleanText
-- --   where cleanText = preprocess text
-- isPalindrome :: T.Text -> Bool
-- isPalindrome text = cleanText == T.reverse cleanText
--   where cleanText = preprocess text

module Lib
  ( isPalindrome
  , preprocess
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