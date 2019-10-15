-- ch06.hs

teams = ["red","yellow","orange","blue","purple"]

-- 6.1

simple x = x
longList = [1..]
stillLongList = simple longList

-- 6.2 

-- 6.3 

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

respond :: String -> String
respond phrase = if '!' `elem` phrase
                 then "wow!"
                 else "uh.. okay"

takeLast :: Int -> [a] -> [a]
-- takeLast n xs = reverse (take n (reverse xs))
takeLast n = reverse . take n . reverse

assignToGroups :: Int -> [a] -> [(Int,a)]
assignToGroups n xs = zip (cycle [1..n]) xs

-- Q6.1

myRepeat :: a -> [a]
myRepeat x = cycle [x]

-- Q6.2

subseq :: Int -> Int -> [a] -> [a]
-- subseq start finish xs = drop start (take finish xs)
subseq start finish = drop start . take finish

-- Q6.3

inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf x xs = x `elem` firstHalf
  where firstHalf  = reverse (drop halfLength (reverse xs))
        halfLength = length xs `div` 2


