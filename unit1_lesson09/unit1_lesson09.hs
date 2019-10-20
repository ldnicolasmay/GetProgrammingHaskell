-- lesson09.hs

import Data.Char


-- 9.1

add3ToAll :: (Num a) => [a] -> [a]
add3ToAll []     = []
add3ToAll (x:xs) = x+3 : add3ToAll xs


mult3ByAll :: (Num a) => [a] -> [a]
mult3ByAll []     = []
mult3ByAll (x:xs) = x*3 : mult3ByAll xs



-- 9.2

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = f x : myMap f xs



-- 9.3

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p []     = []
myFilter p (x:xs) = if p x 
                    then x : myFilter p xs 
                    else myFilter p xs


myRemove :: (a -> Bool) -> [a] -> [a]
myRemove p [] = []
myRemove p (x:xs) = if p x
                    then myRemove p xs
                    else x : myRemove p xs



-- 9.4

myProduct :: (Num a) => [a] -> a
myProduct = foldl (*) 1


concatAll :: [String] -> String
concatAll = foldl (++) ""


sumOfSquares :: (Num a) => [a] -> a
sumOfSquares xs = foldl (+) 0 (map (^2) xs)


myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []
-- myReverse = myFoldl (\acc x -> x:acc) []


-- myFoldl :: (a -> a -> a) -> a -> [a] -> a
myFoldl f acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


-- myFoldr :: (a -> a -> a) -> a -> [a] -> a
myFoldr f acc []     = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)



-- Q9.1

myElem e xs = length (filter (==e) xs) > 0


-- Q9.2

isPalindrome xs = xsNoSpaceNoCaps == reverse xsNoSpaceNoCaps
  where xsNoSpaceNoCaps = map toLower (filter (/=' ') xs)


-- Q9.3

-- harmonic 0 = 0
-- harmonic n = 1 / n + harmSer (n-1)

-- harmonic n = foldl (\acc x -> acc + 1/x) 0 [1..n]

harmonic n = foldl addInverse 0 [1..n]
  where addInverse acc x = acc + 1/x