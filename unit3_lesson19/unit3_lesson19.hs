-- unit3_lesson19.hs



import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List



-- 19.1

groceries :: Map.Map String Int
groceries = Map.fromList [("Milk",1),("Candy Bars",10),("Cheese Blocks",2)]

data Organ = Heart
           | Brain
           | Kidney
           | Spleen
           deriving (Show,Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

{-
λ> Map.lookup 13 organCatalog 
Just Brain
λ> Map.lookup 9 organCatalog 
Nothing
-}



-- 19.2

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents id = Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (== Just organ) available)



-- 19.3

isSomething :: Maybe Organ -> Bool
isSomething Nothing  = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
-- justTheOrgans = filter isSomething availableOrgans
justTheOrgans = filter Maybe.isJust availableOrgans

justTheNothings :: [Maybe Organ]
justTheNothings = filter Maybe.isNothing availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = List.intercalate ", " organList


numOrZero :: Maybe Int -> Int
numOrZero (Just n) = n
numOrZero Nothing  = 0



-- 19.4

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where 
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat organ)    = (Lab,Vat organ)
placeInLocation (Cooler organ) = (Lab,Cooler organ)
placeInLocation (Bag organ)    = (Kitchen, Bag organ)

process :: Organ -> (Location,Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location,Container) -> String
report (location,container) = show container ++ " in the " ++ show location

{-
λ> report (process Heart)
"Heart in a cooler in the Lab"
λ> report (process Spleen)
"Spleen in a bag in the Kitchen"
λ> report (process Brain)
"Brain in a vat in the Lab"
-}

-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ)
--   where organ = Map.lookup id catalog

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing      = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
  where organ = Map.lookup id catalog


-- Quick check 19.3

-- report :: Maybe (Location,Container) -> String
-- report Nothing = "error, nothing"
-- report (Just (location,container)) = 
--   show container ++ " in the " ++ show location


-- Q19.1

emptyDrawers :: [Maybe Organ] -> Int
-- emptyDrawers contents = length . filter Maybe.isNothing $ contents
emptyDrawers = length . filter Maybe.isNothing


-- Q19.2

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)

