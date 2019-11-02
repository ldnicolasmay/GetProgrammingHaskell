-- unit5_lesson32.hs


import Control.Monad
-- import qualified GHC.Base
import Control.Applicative
import Data.Char


-- 32.1 Building Lists with the List Monad

oddSquares :: [Int]
oddSquares = [ x^2 | x <- filter odd [1..20] ]

powersOfTwo :: Int -> [Int]
-- powersOfTwo n = map (2^) [1..n]
-- powersOfTwo n = [ 2^x | x <- [1..n] ]
powersOfTwo n_ = do
  n <- [1..n_]
  return (2^n)

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n_ = do
  n <- [1..n_]
  return (2^n,3^n)

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
  evenValue <- [2,4..n]
  oddValue  <- [1,3..n]
  return (evenValue,oddValue)

squarePairs :: Int -> [(Int,Int)]
squarePairs n_ = do
  n <- [1..n_]
  return (n,n^2)

evensGuard :: Int -> [Int]
evensGuard n_ = do
  n <- [1..n_]
  guard (even n)
  return n

filter_ :: (Monad m, Control.Applicative.Alternative m) => (a -> Bool) -> m a -> m a
-- filter_ :: (Monad m, GHC.Base.Alternative m) => (a -> Bool) -> m a -> m a
filter_ f xs = do
  x <- xs
  guard (f x)
  return x


-- 32.2 List Comprehensions

-- Python list comprehension:
-- [n**2 for n in range(10) if n**2 % 2 == 0]

-- Haskell do notation for equivalent Python list comprehension above
foo :: Int -> [Int]
foo n_ = do
  n <- [0..n_]
  let nSquared = n^2
  guard (even nSquared)
  return (n^2)

-- Haskell list comprehension:
bar :: Int -> [Int]
bar n_ = [ n^2 | n <- [0..n_], n^2 `mod` 2 == 0 ]

-- more practice...
powersOfTwoDo :: Int -> [Int]
powersOfTwoDo n_ = do
  n <- [1..n_]
  return (2^n)

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n_ = [ 2^n | n <- [1..n_] ]

-- powersOfTwoAndThree :: Int -> [(Int,Int)]
-- powersOfTwoAndThree n_ = do
--   n <- [1..n_]
--   return (2^n,3^n)

powersOfTwoAndThreeLC :: Int -> [(Int,Int)]
-- powersOfTwoAndThreeLC n_ = [(2^n,3^n) | n <- [1..n_]]
powersOfTwoAndThreeLC n_ = [ (powerOfTwo,powerOfThree)
                           | n <- [1..n_]
                           , let powerOfTwo = 2^n
                           , let powerOfThree = 3^n
                           ]

allEvenOddsLC :: Int -> [(Int,Int)]
allEvenOddsLC n_ = [ (evenValue,oddValue)
                   | evenValue <- [2,4..n_]
                   , oddValue  <- [1,3..n_]
                   ]

evensGuardLC :: Int -> [Int]
evensGuardLC n_ = [ n | n <- [1..n_], even n ]

-- evensGuardDo :: Int -> [Int]
-- evensGuardDo n_ = do
--   n <- [1..n_]
--   guard (even n)
--   return (n)

colors :: [String]
colors = ["brown","blue","pink","orange"]

clueNames :: [String] -> [String]
-- clueNames names = [ title
--                   | (c : rest) <- names
--                   , let lastName = toUpper c : rest
--                   , let title    = "Mr. " <> lastName
--                   ]
clueNames names = [ "Mr. " <> (toUpper c : rest) | (c : rest) <- names ]



-- Q32.1

-- My solution:
data Month = Jan | Feb | Mar
           | Apr | May | Jun
           | Jul | Aug | Sep
           | Oct | Nov | Dec
           deriving (Read,Show,Eq,Ord,Enum)

monthsDays :: [(Month,Int)]
monthsDays = [ (m,d)
             | m <- [Jan .. Dec]
             , d <- if m == Feb
                    then [1..28]
                    else
                      if m == Apr || m == Jun || m == Sep || m == Nov
                      then [1..30]
                      else [1..31]
             ]

-- Book's solution:
monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

dates :: [Int] -> [Int]
dates ends = [ date | end <- ends, date <- [1 .. end] ]


-- Q32.2

datesDo :: [Int] -> [Int]
datesDo ends = do
  end  <- ends
  date <- [1 .. end]
  return (date)

datesBind :: [Int] -> [Int]
datesBind ends = ends >>= \end -> [1 .. end] >>= \date -> return date


