-- lesson07.hs

myTake :: Int -> [a] -> [a]
myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n-1) xs


fib 0 = [0]
fib 1 = [1,0]
fib n = head prevFib + prevFib !! 1 : prevFib
  where prevFib = fib (n-1)

f 1=[1,0]
f n=let p=f(n-1) in p!!0+p!!1:p
-- main=mapM print . reverse $ f 30
  

-- 7.1

-- 7.2

-- 7.3

myGcd a b = if a `mod` b == 0 
            then b 
            else myGcd b (a `mod` b)

gcd' a b = if remainder == 0
          then b
          else gcd' b remainder
  where remainder = a `mod` b


sayAmount n = case n of 
  1 -> "one"
  2 -> "two"
  _ -> "a bunch"

sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' n = "a bunch"


isEmpty [] = True
isEmpty _  = False


myHead (x:xs) = x
myHead [] = error "No head for empty list"




-- Q7.1

myTail (_:xs) = xs
myTail [] = []


-- Q7.2

-- myGcd a b = if a `mod` b == 0 
--             then b 
--             else myGcd b (a `mod` b)


myGcd' a 0 = a
myGcd' a b = myGcd' b (a `mod` b)