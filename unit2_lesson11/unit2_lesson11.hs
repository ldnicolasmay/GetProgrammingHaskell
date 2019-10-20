-- lesson11.hs

-- myAverage xs = sum xs / lenght xs -- doesn't compile
-- myAverage xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.70,0.80]

letters :: [Char]
letters = ['a','b','c']

-- 11.2

halve :: (Integral a) => a -> a
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

makeAddress :: Int -> String -> String -> (Int,String,String)
makeAddress number street town = (number,street,town)

makeAddressLambda :: Int -> String -> String -> (Int,String,String)
makeAddressLambda = (\number -> (\street -> (\town -> (number,street,town))))

-- makeAddress 123 :  String -> String -> (Int,String,String)
-- makeAddress 123 "Happy St" : String -> (Int,String,String)
-- makeAddress 123 "Happy St" "London" :  (Int,String,String)

ifEven :: (Integral a) => (a -> a) -> a -> a
ifEven f n = if even n
             then f n
             else n


-- 11.3


simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)


-- Q11.1

-- filter :: (a -> Bool) -> [a] -> [a]


-- Q11.2

-- head :: [a] -> a   Answer: No, not unless you returned the head in a list
-- tail :: [a] -> [a] Answer: Yes


-- Q11.3

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init []     = init
myFoldl f init (x:xs) = myFoldl f newInit xs 
  where newInit = f init x






