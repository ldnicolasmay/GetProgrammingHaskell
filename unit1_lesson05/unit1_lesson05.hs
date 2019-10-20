-- ch05.hs

inc :: (Num a) => a -> a
inc n = n + 1

double :: (Num a) => a -> a
double n = n * 2

square :: (Num a) => a -> a
square n = n ^ 2

cube :: (Num a) => a -> a
cube n = n ^ 3

ifEven :: (Integral a) => (a -> a) -> a -> a
ifEven f n = if even n then f n else n

genIfEven :: (Integral a) => (a -> a) -> (a -> a)
genIfEven f = (\n -> ifEven f n) -- <= closure

ifEvenInc :: (Integral a) => a -> a 
-- ifEvenInc n = genIfEven inc n
ifEvenInc = genIfEven inc -- pfs

ifEvenDouble :: (Integral a) => a -> a
ifEvenDouble = genIfEven double

genIfXEven :: (Integral a) => a -> (a -> a) -> a
genIfXEven x = (\f -> if even x then f x else x)

---------------------

getRequestURL :: String -> String -> String -> String -> String
getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder :: String -> (String -> String -> String -> String)
genHostRequestBuilder host =
  (\apiKey resource id -> getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "www.example.com"
-- λ> exampleUrlBuilder "1337hAsk3ll" "book" "1234"
-- "www.example.com/book/1234?token=1337hAsk3ll"

-- type Str = String
-- genApiRequestBuilder :: (Str -> Str -> Str -> Str) -> Str -> Str -> Str -> Str
genApiRequestBuilder hostBuilder apiKey =
  (\resource id -> hostBuilder apiKey resource id)

exampleUrlBuilder' = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"
-- λ> exampleUrlBuilder' "book" "1234"
-- "www.example.com/book/1234?token=1337hAsk3ll"

-- genResourceRequestBuilder apiBuilder resource =
--   (\id -> apiBuilder resource id)

-- exampleUrlBuilder'' = genResourceRequestBuilder exampleUrlBuilder' "book"
-- -- λ> exampleUrlBuilder'' "1234"
-- -- "www.example.com/book/1234?token=1337hAsk3ll"

genApiRequestBuilder' hostBuilder apiKey resource =
  (\id -> hostBuilder apiKey resource id)

exampleUrlBuilder'' = 
  genApiRequestBuilder' exampleUrlBuilder "1337hAsk3ll" "book"
-- λ> exampleUrlBuilder'' "1234"
-- "www.example.com/book/1234?token=1337hAsk3ll"


------------------------


add4 :: (Num a) => a -> a -> a -> a -> a
add4 a b c d = a + b + c + d

addXto3 :: (Num a) => a -> (a -> a -> a -> a)
addXto3 x = (\b c d -> add4 x b c d)

addXYto2 :: (Num a) => a -> a -> (a -> a -> a)
addXYto2 x y = (\c d -> add4 x y c d)

mystery = add4 1
-- λ> mystery 2 3 4
-- 10

anotherMystery = add4 1 2
-- λ> anotherMystery 3 4
-- 10

genResourceRequestBuilder = 
  getRequestURL "www.example.com" "1337hAsk3ll" "book"


--------------------


-- flipBinaryArgs :: (a -> b -> c) -> b -> a -> c
flipBinaryArgs f = (\a b -> f b a)

subtract2 :: (Num a) => a -> a
subtract2 = flip (-) 2


-----------------


-- Q5.1

-- inc :: (Num a) => a -> a
-- inc n = n + 1

-- double :: (Num a) => a -> a
-- double n = n * 2

-- square :: (Num a) => a -> a
-- square n = n ^ 2

-- ifEven :: (Integral a) => (a -> a) -> a -> a
-- ifEven f n = if even n then f n else n

ifEvenInc' :: (Integral a) => a -> a
ifEvenInc' = ifEven inc

ifEvenDouble' :: (Integral a) => a -> a
ifEvenDouble' = ifEven double

ifEvenSquare' :: (Integral a) => a -> a
ifEvenSquare' = ifEven square


-- Q5.2

binaryPartialApplication :: (a -> b -> c) -> b -> a -> c
binaryPartialApplication f x = (\y -> f y x)

