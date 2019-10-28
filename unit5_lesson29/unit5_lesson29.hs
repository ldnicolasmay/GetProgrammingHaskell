-- unit5_lesson29.hs

primesToN :: Int -> [Int]
primesToN n = filter isNotComposite twoThroughN
  where twoThroughN = [2 .. n]
        composites = filter (<=n) (pure (*) <*> twoThroughN <*> twoThroughN)
        isNotComposite = not . (`elem` composites)

data User = User { name    :: String
                 , gamerId :: Int
                 , score   :: Int
                 } deriving (Show)

testNames :: [String]
testNames = [ "John Smith"
            , "Robert'); DROP TABLE Students;--"
            , "Christina NULL"
            , "Randall Munroe"
            , "Pants McBrain"
            ]

testIds :: [Int]
testIds = [ 1337
          , 0123
          , 999999
          ]

testScores :: [Int]
testScores = [ 0
             , 100000
             , -99999
             ]

-- Use Applicative properties of List to nondeterministically generate
-- test data from `testNames`, `testIds`, `testScores`
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores



-- Q29.1

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f x = pure f <*> x



-- Q29.2

-- example :: Int
-- example = (*) ((+) 2 4) 6

example :: Maybe Int
example = pure (*) <*> (pure (+) <*> Just 2 <*> Just 4) <*> Just 6



-- Q29.3

boughtBeer :: [Int]
boughtBeer = [6,12]

drunkBeers :: [Int]
drunkBeers = [4]

visitingFriends :: [Int]
visitingFriends = [2,3]

beersPerPerson :: [Int]
beersPerPerson = [3,4]

totalPeople :: [Int]
totalPeople = pure (+) <*> [2] <*> visitingFriends

totalBeersNeeded :: [Int]
totalBeersNeeded = pure (*) <*> totalPeople <*> beersPerPerson

beersOnHand :: [Int]
beersOnHand = pure (-) <*> boughtBeer <*> drunkBeers

beersToBuy :: [Int]
beersToBuy = pure (-) <*> totalBeersNeeded <*> beersOnHand

qsort :: (Ord a, Num a) => [a] -> [a]
qsort []     = []
-- qsort (x:xs) = smallerXs <> [x] <> biggerXs
--   where smallerXs = qsort $ filter (<=x) xs
--         biggerXs  = qsort $ filter (> x) xs
qsort (x:xs) = qsort smallerXs <> [x] <> biggerXs
  where smallerXs = filter (<=x) xs
        biggerXs  = filter (> x) xs






