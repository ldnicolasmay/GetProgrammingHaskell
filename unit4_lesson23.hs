-- unit4_lesson23.hs

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Semigroup
import qualified Data.Text.IO as TIO

-- 23.1 The Text Type

-- 23.2 Using Data.Text

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

-- main :: IO ()
-- main = do
--   print aWord

-- {-# LANGUAGE TemplateHaskell #-}
-- $ ghc foo.hs -XTemplateHaskell

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "some\ntext for\t you\t\n "

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

textList :: [T.Text]
textList = ["foo","bar"]

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

myTLines :: T.Text -> [T.Text]
myTLines = T.splitOn "\n"

myTUnlines :: [T.Text] -> T.Text
myTUnlines text = T.intercalate "\n" (text <> [""])


-- 23.3 Text and Unicode

dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात् स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces      = T.splitOn query fullText
        highlighted = mconcat ["{",query,"}"]
--


-- 23.4 Text I/O

-- main :: IO ()
-- main = do
--   TIO.putStrLn dharma
--   TIO.putStrLn bgText
--   TIO.putStrLn (highlight dharma bgText)
--


-- Q23.1

-- -- helloPerson :: String -> String
-- -- helloPerson name = "Hello, " ++ name ++ "!"
-- helloPerson :: T.Text -> T.Text
-- helloPerson name = "Hello, " <> name <> "!"

-- -- main :: IO ()
-- -- main = do
-- --   putStrLn "Hello! What's your name?"
-- --   name <- getLine
-- --   let statement = helloPerson name
-- --   putStrLn statement
-- main :: IO ()
-- main = do
--   TIO.putStrLn "Hello! What's your name?"
--   name <- TIO.getLine
--   let statement = helloPerson name
--   TIO.putStrLn statement


-- Q23.2

-- toInts :: String -> [Int]
-- toInts = map read . lines
toInts :: T.Text -> [Int]
toInts = map read . lines . T.unpack

-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let numbers = toInts userInput
--   print (sum numbers)
main :: IO ()
main = do
  userInput <- TIO.getContents   -- userInput :: T.Text <=> "1\n2\n3"
  let numbers = toInts userInput -- 
  print (sum numbers)
