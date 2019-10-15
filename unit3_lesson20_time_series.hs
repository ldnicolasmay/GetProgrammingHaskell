-- unit3_lesson20_time_series.hs


import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe


-- 20.1 Your Data and the TS Data Type


file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)
        ]

file2 :: [(Int,Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8)
        ]
    
file3 :: [(Int,Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7)
        ]
        
file4 :: [(Int,Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6)
        ]

-- Time-series data type: TS
data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where completeTimes  = [minimum times .. maximum times]
        extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes
        timeValueMap   = Map.fromList (zip times values)

-- λ> createTS [1,2,4,5] ['a','b','d','e']
-- TS [1,2,3,4,5] [Just 'a',Just 'b',Nothing,Just 'd',Just 'e']

fileToTs :: [(Int,a)] -> TS a
fileToTs tvPairs = createTS times values
  where (times,values) = unzip tvPairs

showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just val) = mconcat [show time,"|",show val,"\n"]
showTVPair time Nothing    = mconcat [show time,"|NA\n"] 

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

ts1 :: TS Double
ts1 = fileToTs file1

ts2 :: TS Double
ts2 = fileToTs file2

ts3 :: TS Double
ts3 = fileToTs file3

ts4 :: TS Double
ts4 = fileToTs file4




-- 20.2 Stitching together TS Data with Semigroup and Monoid

-- -- My solution
-- extractJust :: Maybe a -> a
-- extractJust (Just x) = x
-- extractJust Nothing  = error "Cannot extract anything from Nothing"

-- combineTS :: TS a -> TS a -> TS a
-- combineTS ts1 (TS [] []) = ts1
-- combineTS (TS [] []) ts2 = ts2
-- combineTS (TS times1 vals1) (TS times2 vals2) = TS completeTimes completeVals
--   where completeTimes = [minimum allTimes .. maximum allTimes]
--         allTimes      = times1 ++ times2
--         ts1Map        = Map.fromList (zip times1 vals1)
--         ts2Map        = Map.fromList (zip times2 vals2)
--         completeVals  = map (\t -> if isJust (Map.lookup t ts2Map)
--                                    then extractJust (Map.lookup t ts2Map)
--                                    else
--                                      if isJust (Map.lookup t ts1Map)
--                                      then extractJust (Map.lookup t ts1Map)
--                                      else Nothing) completeTimes
-- -- ... my solution doesn't work right
        

-- Book's solution
insertMaybePair :: (Ord k) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap
insertMaybePair myMap (key,Just value) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS ts1 (TS [] []) = ts1
combineTS (TS [] []) ts2 = ts2
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes      = mconcat [t1,t2]
        completeTimes  = [minimum bothTimes .. maximum bothTimes]
        tvMap          = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap     = foldl insertMaybePair tvMap     (zip t2 v2)
        combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty  = TS [] []
  mappend = (<>)
  -- mconcat = foldr (<>) mempty 

-- λ> mconcat [ts1,ts2,ts3,ts4]

tsAll :: TS Double
tsAll = mconcat [ts1,ts2,ts3,ts4]



-- 20.3 Performing Calculations on Your Time Series

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (==Nothing) values
                           then Nothing
                           else Just avg
  where justVals   = filter isJust values
        cleanVales = map fromJust justVals
        avg        = mean cleanVales


type CompareFunc a = a -> a -> a  -- type synonym
type TSCompareFunc a = (Int,Maybe a) -> (Int,Maybe a) -> (Int,Maybe a)

makeTSCompare :: (Eq a) => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where newFunc (i1,Nothing) (i2,Nothing) = (i1,Nothing)
        newFunc (_,Nothing) (i,val) = (i,val)
        newFunc (i,val) (_,Nothing) = (i,val)
        newFunc (i1,Just val1) (i2,Just val2) = 
          if func val1 val2 == val1
          then (i1,Just val1)
          else (i2,Just val2)

{-
λ> makeTSCompare max (1,Just 'a') (2,Just 'b')
(2,Just 'b')
λ> makeTSCompare min (1,Just 'a') (2,Just 'b')
(1,Just 'a')
λ> makeTSCompare min (1,Just 'a') (2,Nothing)
(1,Just 'a')
λ> makeTSCompare min (1,Nothing) (2,Nothing)
(1,Nothing)
-}

compareTS :: (Eq a) => (a -> a -> a) -> TS a -> Maybe (Int,Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (==Nothing) values
                                   then Nothing
                                   else Just best
  where pairs = zip times values
        best  = foldl (makeTSCompare func) (0,Nothing) pairs
--

minTS :: (Ord a) => TS a -> Maybe (Int,Maybe a)
minTS = compareTS min

maxTS :: (Ord a) => TS a -> Maybe (Int,Maybe a)
maxTS = compareTS max

{-
λ> minTS tsAll 
Just (4,Just 198.9)
λ> maxTS tsAll 
Just (28,Just 223.8)
-}



-- 20.4 Transforming Time Series

diffPair :: (Num a) => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing         = Nothing
diffPair Nothing _         = Nothing
diffPair (Just x) (Just y) = Just (y - x)

diffTS :: (Num a) => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
  where shiftValues = tail values
        diffValues  = zipWith diffPair values shiftValues


--
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if Nothing `elem` vals
                 then Nothing
                 else Just avg
  where avg = mean (map fromJust vals)

--
movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg vals n = if length nextVals == n
                   then meanMaybe nextVals:movingAvg restVals n
                   else []
  where nextVals = take n vals
        restVals = tail vals

--
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where movAvg         = movingAvg values n
        nothings       = replicate (n `div` 2) Nothing
        smoothedValues = mconcat [nothings,movAvg,nothings]













