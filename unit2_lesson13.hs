-- lesson13.hs

simple x = x

-- 13.3 

addThenDouble :: (Num a) => a -> a -> a
addThenDouble x y = (x + y) * 2

-- 13.4

-- def.n of a type class:
-- class TypeName a where
--   fun1 :: a -> a
--   fun2 :: a -> String
--   fun3 :: a -> a -> Bool

class Describable a where
  describe :: a -> String

-- 13.6

-- data Icecream = Chocolate | Vanilla

-- 13.7

-- data Icecream = Chocolate | Vanilla deriving (Show)

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)


-- Q13.1

-- Int is -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
-- Word is 0 to 18,446,744,073,709,551,615


-- Q13.2

-- The `Enum` typeclass allows for things to be ordered and/or put in ranges.
-- [1..10], ['a'..'z']


-- Q13.3

-- cycleSucc :: (Bounded a, Enum a, ? a) => a -> a
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc x = if x == maxBound -- type of maxBound inferred by use of `==`
              then minBound
              else succ x

