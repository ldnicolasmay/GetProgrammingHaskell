sumSquareOrSquareSum :: (Num a, Ord a) => a -> a -> a
-- sumSquareOrSquareSum x y = if sumSquare > squareSum
--                            then sumSquare
--                            else squareSum
--                            where sumSquare = x^2 + y^2
--                                  squareSum = (x+y)^2
-- sumSquareOrSquareSum x y = 
--   (\sumSquare squareSum -> if sumSquare > squareSum
--                            then sumSquare
--                            else squareSum) (x^2 + y^2) ((x+y)^2) 
sumSquareOrSquareSum x y = 
  let sumSquare = x^2 + y^2
      squareSum = (x+y)^2
  in  if sumSquare > squareSum 
      then sumSquare 
      else squareSum

doubleDouble :: (Num a) => a -> a
-- doubleDouble x = dubs * 2
--   where dubs = x * 2
doubleDouble x = (\y -> y * 2) (x * 2)

overwrite :: (Num a) => a -> a
-- overwrite x = (\x -> 4) ((\x -> 3) ((\x -> 2) x))
overwrite x = (\x -> (\x -> (\x -> 4) 3) 2) x 

counter :: (Num a) => a -> a
-- counter x = (\x -> x+1) ((\x -> x+1) x)
-- counter x = (\x -> (\x -> x+1) x+1) x
-- counter x = (\x -> x+1) . (\x -> x+1) $ x
counter = (\x -> x+1) . (\x -> x+1)  -- pfs