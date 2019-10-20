-- lesson08.hs

-- 8.1

-- 1. Identify the end goal(s).
-- 2. Determine what happens when a goal is reached.
-- 3. List all alternate possibilities.
-- 4. Determine your "rinse and repeat" process.
-- 5. Ensure that each alternative moves you toward the goal.


-- 8.2

myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs


myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n-1) xs


myCycle :: [a] -> [a]
myCycle [] = error "Empty list"
-- myCycle xs = xs ++ myCycle xs
myCycle (x:xs) = x : myCycle (xs ++ [x])


ackerman :: Int -> Int -> Int
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))


collatz :: Int -> Int
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)


-- Q8.1

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Q8.2

fastFib m _ 1 = m
fastFib m n f = fastFib n (m+n) (f-1)

f a _ 0 = a
f a b n = f b (a+b) (n-1)
-- main = mapM print $ map (\n -> f 0 1 n) [1..30]
