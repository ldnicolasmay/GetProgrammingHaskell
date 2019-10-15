-- unit3_lesson20_time_series_2.hs

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

data TimeSeries a = TimeSeries [Int] [Maybe a]

--
createTS :: [Int] -> [a] -> TimeSeries a
createTS times vals = TimeSeries completeTimes completeVals
  where completeTimes = [minimum times .. maximum times]
        completeVals  = map (\t -> Map.lookup t timeValMap) completeTimes
        timeValMap    = Map.fromList (zip times vals)

--
fileToTs :: [(Int,a)] -> TimeSeries a
fileToTs timeValPairs = createTS times vals
  where (times,vals) = unzip timeValPairs

--
showTimeValPair :: (Show a) => Int -> Maybe a -> String
showTimeValPair time (Just val) = mconcat [show time,"|",show val,"\n"]
showTimeValPair time Nothing    = mconcat [show time,"|","NA\n"]

--
instance Show a => Show (TimeSeries a) where
  show (TimeSeries times vals) = mconcat rows
    where rows = zipWith showTimeValPair times vals

--
ts1 :: TimeSeries Double
ts1 = fileToTs file1

ts2 :: TimeSeries Double
ts2 = fileToTs file2

ts3 :: TimeSeries Double
ts3 = fileToTs file3

ts4 :: TimeSeries Double
ts4 = fileToTs file4



-- 20.2 Stitching Together TS Data with Semigroup and Monoid

--
combineTS :: TimeSeries a -> TimeSeries a -> TimeSeries a
combineTS (TimeSeries t1s v1s) (TimeSeries [] []) = TimeSeries t1s v1s
combineTS (TimeSeries [] []) (TimeSeries t2s v2s) = TimeSeries t2s v2s
combineTS (TimeSeries t1s v1s) (TimeSeries t2s v2s) =
  TimeSeries completeTimes completeVals
  where completeTimes = [minimum catTimes .. maximum catTimes]
        catTimes      = t1s <> t2s
        completeVals  = map (\t -> Map.lookup t ts2MaybeZip) completeTimes
        ts1MaybeZip   = foldl insertMaybePair Map.empty ts2Zip
        ts2MaybeZip   = foldl insertMaybePair ts1MaybeZip ts1Zip
        ts1Zip        = zip t1s v1s
        ts2Zip        = zip t2s v2s

--
insertMaybePair :: (Ord k) => Map.Map k v -> (k,Maybe v) -> Map.Map k v
insertMaybePair myMap (_,Nothing) = myMap
insertMaybePair myMap (k,Just v)  = Map.insert k v myMap

--
instance Semigroup (TimeSeries a) where
  (<>) = combineTS

--
instance Monoid (TimeSeries a) where
  mempty  = TimeSeries [] []
  mappend = (<>)
  mconcat = foldr mappend mempty

--
tsAll :: TimeSeries Double
tsAll = mconcat [ts1,ts2,ts3,ts4]



-- 20.3 Performing Calculations on Your Time Series

-- 
mean :: (Real a) => [a] -> Double
mean xs = realToFrac (sum xs) / realToFrac (length xs)

--
meanTS :: (Real a) => TimeSeries a -> Maybe Double
meanTS (TimeSeries _ [])   = Nothing
meanTS (TimeSeries _ vals) = if all (==Nothing) vals
                             then Nothing 
                             else Just (mean filteredVals)
  where filteredVals = map fromJust justVals
        justVals     = filter isJust vals

--

