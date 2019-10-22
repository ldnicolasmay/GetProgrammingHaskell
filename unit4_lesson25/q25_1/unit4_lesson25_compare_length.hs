-- unit4_lesson25_compare_length.hs


{-# LANGUAGE OverloadedStrings #-}

import           System.Environment
import qualified Data.Text          as T   (Text,length)
import qualified Data.Text.IO       as TIO (readFile,putStrLn)
import qualified Data.ByteString    as B   (readFile,length)


compareText :: Int -> Int -> T.Text
compareText textLen byteLen = case compare textLen byteLen of
  LT -> "text length < byte length"
  EQ -> "text length = byte length"
  GT -> "text length > byte length"


main :: IO ()
main = do
  args <- getArgs           -- args :: [String]
  let fileName = head args  -- fileName :: String
  --
  fileAsText    <- TIO.readFile fileName  -- fileAsText :: T.Text
  let textLength = T.length fileAsText    -- textLength :: Int
  --
  fileAsByte    <- B.readFile fileName  -- fileAsByte :: ByteString
  let byteLength = B.length fileAsByte  -- byteLength :: Int
  --
  let comparisonText = compareText textLength byteLength
  TIO.putStrLn comparisonText
