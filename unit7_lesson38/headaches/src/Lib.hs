module Lib
    ( someFunc
    , myTake
    , myTakePM
    , myHead
    , maybeHead
    , myTakeSafer
    , eitherHead
    , intExample
    , intExampleEmpty
    , charExample
    , charExampleEmpty
    , isPrime
    , displayResult
    ) where

import Data.Char (isDigit)

-- 38.1 Head, Partial Functions, and Errors

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myTake :: Int -> [a] -> [a]
myTake 0 _  = []
-- myTake _ [] = []
myTake n xs = head xs : myTake (n-1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _      = []
myTakePM _ []     = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x


-- 38.2 Handling Partial Functions with Maybe

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- (+2) <$> maybeHead [1]
-- (+2) <$> maybeHead []

-- (+) <$> maybeHead [1,2,3] <*> maybeHead [4,5,6] -- Just 5
-- (+) <$> maybeHead [1,2,3] <*> maybeHead []      -- Nothing
-- (+) <$> maybeHead []      <*> maybeHead [4,5,6] -- Nothing


myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))


-- 38.3 Introducing the Either Type

primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

-- isPrime :: Int -> Maybe Bool
-- isPrime n | n < 2     = Nothing
--           | n > maxN  = Nothing
--           | otherwise = Just (n `elem` primes)

-- data Either a b = Left a | Right b
-- data Either a b = Fail a | Correct b

eitherHead :: [a] -> Either String a
eitherHead []     = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat"

charExampleEmpty :: [Char]
charExampleEmpty = ""

-- λ> eitherHead intExample
-- Right 1
-- λ> eitherHead intExampleEmpty 
-- Left "There is no head because the list is empty"
-- λ> eitherHead charExample
-- Right 'c'
-- λ> eitherHead charExampleEmpty 
-- Left "There is no head because the list is empty"

-- λ> (+1) <$> eitherHead intExample
-- Right 2
-- λ> (+1) <$> eitherHead intExampleEmpty 
-- Left "There is no head because the list is empty"

-- isPrime :: Int -> Either String Bool
-- isPrime n
--   | n < 2     = Left "Numbers less than 2 are not candidates for primes"
--   | n > maxN  = Left "Value exceeds limits of prime checker"
--   | otherwise = Right (n `elem` primes)

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge     = "Value exceeds max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2     = Left InvalidValue
  | n > maxN  = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)      = "It's prime"
displayResult (Right False)     = "It's composite"
displayResult (Left primeError) = show primeError


-- Q38.1

allDigits :: String -> Bool
allDigits = all isDigit

notAllDigits :: String -> Bool
notAllDigits = not . allDigits

addStrInts :: String -> String -> Either String Int
addStrInts str1 str2
  | notAllDigits str1 && allDigits str2    = Left "First value can't be parsed"
  | allDigits str1    && notAllDigits str2 = Left "Second value can't be parsed"
  | notAllDigits str1 && notAllDigits str2 = Left "Neither value can be parsed"
  | otherwise                              = Right (int1 + int2)
  where int1 = read str1 :: Int
        int2 = read str2 :: Int


-- Q38.2

saferSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
saferSucc x = if x == maxBound
              then Nothing
              else Just (succ x)

saferTail :: [a] -> [a]
saferTail []     = []
saferTail (x:xs) = xs

-- saferLast :: (Enum a, Bounded a, Eq a) => [a] -> Either String a
-- saferLast [] = Left "empty list"
-- saferLast xs = 
-- The answer to this `saferLast` in the book is screwy... skipping it.

