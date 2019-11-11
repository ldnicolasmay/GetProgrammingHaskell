-- import Lib
-- import Test.QuickCheck
-- import Data.Char (isPunctuation)
import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation,isSpace)
import Data.Text as T (Text,filter,reverse,toLower)


-- assert :: Bool -> String -> String -> IO ()
-- assert test passStatement failStatement = if test
--                                           then putStrLn passStatement
--                                           else putStrLn failStatement
--
-- main :: IO ()
-- main = do
--   putStrLn "Running tests..."
--   assert (isPalindrome "racecar")     "passed 'racecar'"   "FAIL: 'racecar'"
--   assert (isPalindrome "racecar!")    "passed 'racecar!'"  "FAIL: 'racecar!'"
--   assert ((not . isPalindrome) "cat") "passed 'cat'"       "FAIL: 'cat'"
--   assert (isPalindrome "racecar.")    "passed 'racecar.'"  "FAIL: 'racecar.'"
--   assert (isPalindrome ":racecar:")   "passed ':racecar:'" "FAIL: ':racecar:'"
--   putStrLn "done!"

-- prop_punctuationInvariant :: String -> Bool
-- prop_punctuationInvariant text = preprocess text == preprocess noPuncText
--   where noPuncText = filter (not . isPunctuation) text
prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where noPuncText = T.filter (not . isPunctuation) text

-- prop_reverseInvariant :: String -> Bool
-- prop_reverseInvariant text = isPalindrome text == isPalindrome reverseText
--   where reverseText = reverse text
prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == isPalindrome reverseText
  where reverseText = T.reverse text

prop_whiteSpaceInvariant :: T.Text -> Bool
prop_whiteSpaceInvariant text = preprocess text == preprocess noWhiteSpaceText
  where noWhiteSpaceText = T.filter (not . isSpace) text

prop_caseInvariant :: T.Text -> Bool
prop_caseInvariant text = preprocess text == preprocess lowerText
  where lowerText = T.toLower text

main :: IO ()
main = do
  putStrLn ""
  -- quickCheck prop_punctuationInvariant
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
  -- quickCheck prop_reverseInvariant
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_reverseInvariant
  -- quickCheck prop_whiteSpaceInvariant
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_whiteSpaceInvariant
  -- quickCheck prop_caseInvariant
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_caseInvariant
  putStrLn "done!"

-- To use QuickCHeck with more types, install `quickcheck-instances`
-- $ stack install quickcheck-instances

