-- unit3_lesson17.hs

-- 17.1

import Data.List
import Data.Semigroup

myLast :: [a] -> a
myLast = head . reverse

myMin :: (Ord a) => [a] -> a
myMin = head . sort

myMax :: (Ord a) => [a] -> a
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr (\x acc -> acc && p x) True          -- mine
-- myAll testFunc = (foldr (&&) True) . (map testFund)  -- book's
-- The book's is confusing af

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x acc -> acc || p x) False



-- 17.2 Combining Like Types: Semigroups

-- The `<>` operator

-- Integer type as an instance of the Semigroup typeclass
instance Semigroup Integer where
  (<>) x y = x + y

-- -- Here's the type signature for `<>`
-- (<>) :: Semigroup a => a -> a -> a

data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           deriving (Show,Eq)

-- instance Semigroup Color where
--   (<>) Red Yellow  = Orange
--   (<>) Yellow Red  = Orange
--   (<>) Yellow Blue = Green
--   (<>) Blue Yellow = Green
--   (<>) Red Blue    = Purple
--   (<>) Blue Red    = Purple
--   (<>) a    b      = if a == b then a else Brown

-- Semigroup law: Associativity
-- x <> (y <> z) = (x <> y) <> z

-- λ> Red <> Yellow
-- Orange
-- λ> Red <> Blue
-- Purple
-- -- Not associative as a Semigroup should be
-- λ> (Green <> Blue) <> Yellow
-- Brown
-- λ> Green <> (Blue <> Yellow)
-- Green

-- We'll fix the non-associativity with guards
instance Semigroup Color where
  (<>) Red Yellow  = Orange
  (<>) Yellow Red  = Orange
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Blue    = Purple
  (<>) Blue Red    = Purple
  (<>) a    b      | a == b = a
                   | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
                   | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
                   | all (`elem` [Red, Blue, Purple]  ) [a,b] = Purple
                   | otherwise                                = Brown

-- λ> (Green <> Blue) <> Yellow
-- Green
-- λ> Green <> (Blue <> Yellow)
-- Green
-- λ> (Red <> Blue) <> Yellow
-- Brown
-- λ> Red <> (Blue <> Yellow)
-- Brown



-- 17.3 Composing with Identity: Monoids

-- The definition of the Monoid typeclass should be:
-- class Semigroup a => Monoid a where
--   identity :: a

-- ... but that's not the case for historical reasons.
-- Instead it's this:
-- class Monoid a where
--   mempty :: a             -- analog of `identity`
--   mappend :: a -> a -> a  -- analog of `<>`
--   mconcat :: [a] -> a

-- λ> [1,2,3] ++ []         -- `mappend` synonym for list instance of Monoid
-- [1,2,3]
-- λ> [1,2,3] <> []         -- `<>` for list instance of Semigroup
-- [1,2,3]
-- λ> [1,2,3] `mappend` []  -- `mappend` for list instance of Monoid
-- [1,2,3]


-- `mconcat` for combining multiple monoids at once

-- λ> mconcat ["a","b","c","d","e","f"]
-- "abcdef"
-- λ> mconcat ["Does ","this ","make ","sense?"]
-- "Does this make sense?"
-- λ> mconcat [["a","b"],["c","d"],["e","f"]]
-- ["a","b","c","d","e","f"]

-- λ> mconcat [Sum 1,Sum 2,Sum 3,Sum 4]
-- Sum {getSum = 10}

-- `mconcat`'s default implementation comes for free once `mappend` and `mempty`
-- are defined for an instance of the Monoid typeclass... because `mconcat`
-- only depends on `mappend` and `mempty`:
-- mconcat :: (Monoid a) => [a] -> a
-- mconcat = foldr mappend mempty 

-- Monoid laws: Identity, Associativity
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z
-- Identity and associativity allow for `mconcat` to always be true:
-- mconcat = foldr mappend mempty

-- Building probability tables with monoids

{-
| Event | Probability |
+-------+-------------+
| Heads | 0.5         |
| Tails | 0.5         |
-}

type Events = [String]
type Probs  = [Double]

data ProbTable = ProbTable Events Probs -- deriving (Show,Eq)

createProbTable :: Events -> Probs -> ProbTable
createProbTable events probs = ProbTable events normalizedProbs
  where normalizedProbs = map (/ sumProbs) probs
        sumProbs        = sum probs

myProbTable1 = ProbTable ["Heads","Tails"] [0.5,0.5]
myProbTable2 = createProbTable ["Heads","Tails"] [0.5,0.5]

showPair :: String -> Double -> String
showPair event prob = mconcat [event," | ",show prob,"\n"]

instance Show ProbTable where
  show (ProbTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs


cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
-- cartCombine f xs ys = [ f x y | x <- xs, y <- ys ]
cartCombine f xs ys = xs >>= \x -> ys >>= \y -> return (f x y)
-- cartCombine f xs ys = zipWith f xs ys

combineEvents :: Events -> Events -> Events
-- combineEvents e1 e2 = cartCombine combiner e1 e2
--   where combiner a b = mconcat [a,"-",b]
combineEvents e1s e2s = 
  e1s >>= \e1 -> e2s >>= \e2 -> return (mconcat [e1,"-",e2])
-- combineEvents e1s e2s = do
--   e1 <- e1s
--   e2 <- e2s
--   return (mconcat [e1,"-",e2])

combineProbs :: Probs -> Probs -> Probs
-- combineProbs p1s p2s = 
--   p1s >>= \p1 -> p2s >>= \p2 -> return (p1 * p2)
combineProbs = cartCombine (*)


-- We can now make ProbTable an instance of Semigroup
instance Semigroup ProbTable where
  -- (<>) (ProbTable e1s p1s) (ProbTable [] [])   = createProbTable e1s p1s
  -- (<>) (ProbTable []  [])  (ProbTable e2s p2s) = createProbTable e2s p2s
  (<>) probTable1 (ProbTable [] []) = probTable1
  (<>) (ProbTable [] []) probTable2 = probTable2
  (<>) (ProbTable e1s p1s) (ProbTable e2s p2s) = 
    createProbTable combinedEvents combinedProbs 
      where combinedEvents = combineEvents e1s e2s
            combinedProbs  = combineProbs  p1s p2s

--
instance Monoid ProbTable where
  mempty  = ProbTable [] []
  mappend = (<>)
  -- a default implementation of `mconcat` is defined for free:
  -- mconcat = foldr mappend mempty

coin :: ProbTable
coin = createProbTable ["heads","tails"] [0.5,0.5]

spinner :: ProbTable
spinner = createProbTable ["red","green","blue"] [0.1,0.2,0.7]

-- λ> coin <> spinner
-- heads-red | 5.0e-2
-- heads-green | 0.1
-- heads-blue | 0.35
-- tails-red | 5.0e-2
-- tails-green | 0.1
-- tails-blue | 0.35

-- λ> coin <> coin <> coin
-- heads-heads-heads | 0.125
-- heads-heads-tails | 0.125
-- heads-tails-heads | 0.125
-- heads-tails-tails | 0.125
-- tails-heads-heads | 0.125
-- tails-heads-tails | 0.125
-- tails-tails-heads | 0.125
-- tails-tails-tails | 0.125

-- λ> mconcat [coin,coin,coin] -- foldr (<>) mempty [coin,coin,coin]
-- heads-heads-heads | 0.125
-- heads-heads-tails | 0.125
-- heads-tails-heads | 0.125
-- heads-tails-tails | 0.125
-- tails-heads-heads | 0.125
-- tails-heads-tails | 0.125
-- tails-tails-heads | 0.125
-- tails-tails-tails | 0.125



-- Q17.1

data MColor = MClear
            | MRed
            | MYellow
            | MBlue
            | MGreen
            | MPurple
            | MOrange
            | MBrown
            deriving (Show,Eq)

instance Semigroup MColor where
  (<>) mc MClear   = mc
  (<>) MClear mc   = mc
  (<>) MRed MYellow  = MOrange
  (<>) MYellow MRed  = MOrange
  (<>) MYellow MBlue = MGreen
  (<>) MBlue MYellow = MGreen
  (<>) MRed MBlue    = MPurple
  (<>) MBlue MRed    = MPurple
  (<>) mcA  mcB    | mcA == mcB = mcA
                   | all (`elem` [MRed, MYellow, MOrange]) [mcA,mcB] = MOrange
                   | all (`elem` [MBlue, MYellow, MGreen]) [mcA,mcB] = MGreen
                   | all (`elem` [MRed, MBlue, MPurple]  ) [mcA,mcB] = MPurple
                   | otherwise = MBrown

instance Monoid MColor where
  mempty  = MClear
  mappend = (<>)

-- λ> MClear <> MBlue
-- MBlue
-- λ> MClear <> MClear
-- MClear
-- λ> (MClear <> MClear) <> MBlue
-- MBlue
-- λ> foldr (<>) MClear [MBlue,MYellow]
-- MGreen
-- λ> mconcat [MBlue,MYellow]
-- MGreen
-- λ> mconcat [MBlue,MYellow,MYellow]
-- MGreen
-- λ> mconcat [MBlue,MYellow,MYellow,MBlue]
-- MGreen
-- λ> (MGreen <> MBlue) <> MYellow
-- MGreen
-- λ> MGreen <> (MBlue <> MYellow)
-- MGreen


-- Q17.2

-- data MEvents = MEvents [String]
-- data MProbs  = MProbs [Double]

newtype MEvents = MEvents [String] deriving (Show)
newtype MProbs  = MProbs  [Double] deriving (Show)

-- data ProbTable = ProbTable MEvents MProbs

instance Semigroup MEvents where
  (<>) ev1s (MEvents []) = ev1s
  (<>) (MEvents []) ev2s = ev2s
  (<>) (MEvents ev1s) (MEvents ev2s) = 
    MEvents (ev1s >>= \ev1 -> ev2s >>= \ev2 -> return (mconcat [ev1,"-",ev2]))
    -- MEvents (do 
    --   ev1 <- ev1s
    --   ev2 <- ev2s 
    --   return (mconcat [ev1,"-",ev2]))

instance Semigroup MProbs where
  (<>) pt1s (MProbs []) = pt1s
  (<>) (MProbs []) pt2s = pt2s
  (<>) (MProbs pt1s) (MProbs pt2s) =
    MProbs
      (pt1s >>= \pt1 -> pt2s >>= \pt2 -> return (pt1 * pt2))

instance Monoid MEvents where
  mempty  = MEvents []
  mappend = (<>)

instance Monoid MProbs where
  mempty  = MProbs []
  mappend = (<>)
