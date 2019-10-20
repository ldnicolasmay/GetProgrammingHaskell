-- unit3_lesson18.hs

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import qualified Data.Map as Map
-- `qualified` means we avoid function name conflicts (w/ Prelude) by having
-- to call all functions in the module with `Data.Map` or, b/c we've added
-- an `as`, `Map`. So, to use Data.Map's `filter` function, we'd have to call
-- `Data.Map.filter` or in this case just `Map.filter`

-- 18.1 Types that Take Arguments

-- data Box a = Box a deriving (Show)
newtype Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x


data Triplet a = Triplet a a a deriving (Show)

type Point3D = Triplet Double -- type synonym

-- a3DPoint :: Triplet Double
a3DPoint :: Point3D
a3DPoint = Triplet 0.1 53.2 12.3


data Doublet a = Doublet a a deriving (Show)
type Point2D = Doublet Double

-- a2DPoint :: Doublet Double
a2DPoint :: Point2D
a2DPoint = Doublet 6.7 3.1


type FullName = Triplet String

-- name1 :: Triplet String
name1 :: FullName 
name1 = Triplet "Howard" "Phillips" "Lovecraft"


type Initials = Triplet Char

-- inits1 :: Triplet Char
inits1 :: Initials
inits1 = Triplet 'H' 'P' 'L'


-- Write functions that work on the parametized type `Triple`

first :: Triplet a -> a
first (Triplet x _ _) = x

second :: Triplet a -> a
second (Triplet _ y _) = y

third :: Triplet a -> a
third (Triplet _ _ z) = z

toList :: Triplet a -> [a]
toList (Triplet x y z) = [x,y,z]


transform :: (a -> a) -> Triplet a -> Triplet a
transform f (Triplet x y z) = Triplet (f x) (f y) (f z)

{-
-- Why not write the below version of `transform`?
transform :: (a -> b) -> Triplet a -> Triplet b
transform f (Triplet x y z) = Triplet (f x) (f y) (f z)
-- It'd allow functions like these that converts Strings to Doubles:
transform (\t -> read t :: Double) (Triplet "3.1" "2.7" "8.75")
-}

{-
λ> transform (*2) a3DPoint 
Triplet 0.2 106.4 24.6
λ> transform reverse name1
Triplet "drawoH" "spillihP" "tfarcevoL"
λ> toList (transform toLower inits1)
"hpl"
λ> transform (\n -> map toUpper n) name1 
Triplet "HOWARD" "PHILLIPS" "LOVECRAFT"
-}

{-
Quick Check 18.2
What’s the difference between transform and the map function for lists? 
(Hint: Look up the type signature of map again.)

Answer:
λ> :t map
map :: (a -> b) -> [a] -> [b]
λ> :t transform 
transform :: (a -> b) -> Triplet a -> Triplet b
-}

-- Defining our own list (same exercise in LYaHfGG)
-- `Foldable` being derived is made possible by the language pragma
-- thingy at the top: {-# LANGUAGE DeriveFoldable #-}


-- Implementing a linked list from scratch in Haskell
data List a = Empty 
            | Cons a (List a) 
            deriving (Read,Foldable,Traversable)

instance Show a => Show (List a) where
  show Empty = "MT"
  show (Cons x xs) = show x <> "-" <> show xs

instance (Eq a) => Eq (List a) where
  (==) (Cons x xs) (Cons y ys) = (==) x y && (==) xs ys 
  (==) Empty Empty             = True
  (==) _ _                     = False
  
instance (Ord a) => Ord (List a) where
  compare (Cons x xs) (Cons y ys) = case compare x y of
                                    LT -> LT
                                    EQ -> compare xs ys
                                    GT -> GT
  compare Empty Empty = EQ
  compare xs Empty    = GT
  compare Empty ys    = LT

instance Semigroup (List a) where
  (<>) xs Empty       = xs
  (<>) Empty ys       = ys
  (<>) (Cons x xs) ys = Cons x ((<>) xs ys)

instance Monoid (List a) where
  mempty  = Empty
  mappend = (<>)
  mconcat = foldr mappend Empty

mapL :: (a -> b) -> List a -> List b
mapL _ Empty       = Empty
mapL f (Cons x xs) = Cons (f x) (mapL f xs)

instance Functor List where
  fmap = mapL

instance Applicative List where
  pure x      = Cons x Empty
  (<*>) fs xs = fs >>= \f -> xs >>= \x -> return (f x)

instance Monad List where
  return x   = Cons x Empty
  (>>=) xs f = foldr (<>) Empty (mapL f xs)



headL :: List a -> a
headL (Cons x ys) = x
headL Empty = error "Cannot get head from empty List"

tailL :: List a -> a
tailL (Cons x Empty) = x
tailL (Cons x xs)    = tailL xs
tailL Empty = error "Cannot get tail from empty List"


builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1a :: List Int
ourListEx1a = Cons 1 (Cons 2 (Cons 3 Empty))

ourListEx1b :: List Int
ourListEx1b = Cons 4 (Cons 5 (Cons 6 Empty)) 

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
-- ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))
ourListEx2 = 'c' `Cons` ('a' `Cons` ('t' `Cons` Empty))

ourListInList :: List (List Int)
ourListInList = Cons ourListEx1a (Cons ourListEx1b Empty)



-- 18.2 Types with More than One Parameter

itemCount1 :: (String,Int)
itemCount1 = ("Erasers",25)

itemCount2 :: (String,Int)
itemCount2 = ("Pencils",25)

itemCount3 :: (String,Int)
itemCount3 = ("Pens",13)

itemInventory :: [(String,Int)]
itemInventory = [itemCount1,itemCount2,itemCount3]

-- 18.2.2 Kinds: types of teyps

{-
λ> :k Int
Int :: *
λ> :k []
[] :: * -> *
λ> :k List
List :: * -> *
λ> :k (,)
(,) :: * -> * -> *
λ> :k (,,)
(,,) :: * -> * -> * -> *
λ> :k Map.Map 
Map.Map :: * -> * -> *
-}

data Organ = Heart
           | Brain
           | Kidney
           | Spleen
           | Lung
           deriving (Show,Eq,Ord,Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

{-
λ> Map.lookup 7 organCatalog 
Just Heart
λ> :t Map.lookup
Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a
λ> Map.lookup 9 organCatalog 
Nothing
-}



-- Q18.1 

tripletMap :: (a -> b) -> Triplet a -> Triplet b
tripletMap f (Triplet x y z) = Triplet (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)


-- Q18.2

organs2 :: [Organ]
organs2 = [ Brain, Brain, Brain
          , Kidney, Kidney, Kidney, Kidney, Kidney, Kidney, Kidney
          , Heart, Heart, Heart, Heart, Heart
          , Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen
          , Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen
          , Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen
          , Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen
          , Spleen, Spleen, Spleen, Spleen, Spleen, Spleen, Spleen
          ]

organs' :: [Organ]
organs' = [Heart .. Spleen]

counts' :: [Int]
counts' = map (\o -> length (filter (==o) organs2)) organs'

organCounts' :: [(Organ,Int)]
organCounts' = zip organs' counts'

organInventory' :: Map.Map Organ Int
organInventory' = Map.fromList organCounts'

{-
λ> Map.lookup Heart organInventory' 
Just 5
λ> Map.lookup Spleen organInventory'
Just 39
λ> Map.lookup Lung organInventory'
Nothing
-}




