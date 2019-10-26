-- unit5_lesson27.hs


import qualified Data.Map as Map


-- 271. An Example: Computing in a Maybe

incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing  = Nothing
incMaybe (Just n) = Just (n+1)

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

inc :: Int -> Int
inc = (+1)

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing  = Nothing
reverseMaybe (Just s) = Just (reverse s)


-- 27.2 Using Functions in Conext with the Functor Type Class

-- :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- Maybe parameterized type as an instance of Functor type class:
-- instance Functor Maybe where
--   fmap f Nothing  = Nothin
--   fmap f (Just x) = Just (f x)

-- fmap inc successfulRequest
-- fmap inc failedRequest
-- inc <$> successfulRequest
-- inc <$> failedRequest

-- λ> reverse <$> (Just "foobar")
-- Just "raboof"
-- λ> reverse <$> Nothing
-- Nothing


-- 27.3 Functors Are Everywhere!

data RobotPart = RobotPart { name :: String
                           , description:: String
                           , cost :: Double
                           , count :: Int
                           } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , description = "left arm for face punching!"
                    , cost = 1000.00
                    , count = 3
                    }

rightArm :: RobotPart
rightArm = RobotPart { name = "right arm"
                     , description = "right arm for kind hand gestures"
                     , cost = 1025.00
                     , count = 5
                     }

robotHead :: RobotPart
robotHead = RobotPart { name = "robot head"
                      , description = "this head looks mad"
                      , cost = 5092.25
                      , count = 2
                      }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>"
                          , partName
                          ,"</h2>"
                          , "<p>"
                          , "<h3>desc</h3>"
                          , partDesc
                          , "</p>"
                          , "<p>"
                          , "<h3>cost</h3>"
                          , partCost
                          , "</p>"
                          , "<p>"
                          , "<h3>count</h3>"
                          , partCount
                          , "</p>"
                          ]
  where partName  = name part
        partDesc  = description part
        partCost  = show (cost part)
        partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where keys    = [1,2,3]
        vals    = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
-- allParts = map snd (Map.toList partsDB)
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts -- <$> == fmap == map (for lists)

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO


-- Q27.1

data Box a = Box a deriving Show


instance Functor Box where
  fmap f (Box x) = Box (f x)

-- morePresents 0 (Box 'a') => Box []
-- morePresents 1 (Box 'a') => Box ['a']
-- morePresents 2 (Box 'a') => Box ['a','a']
-- morePresents 3 (Box 'a') => Box ['a','a','a']

morePresentsHelper :: Int -> a -> [a]
morePresentsHelper 0 _ = []
morePresentsHelper n x = x : morePresentsHelper (n-1) x

morePresents :: Int -> Box a -> Box [a]
morePresents 0 _ = Box []
morePresents n x = morePresentsHelper n <$> x


-- Q27.2

myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- λ> fmap wrap myBox 
-- Box (Box 1)
-- λ> fmap unwrap (fmap wrap myBox)
-- Box 1


-- Q27.3

-- main :: IO ()
-- main = do
--   putStrLn "Enter a RobotPart part number: "
--   userInput <- getLine
--   let userNum = read userInput :: Int
--   let maybeRobotPart = Map.lookup userNum partsDB -- robotPart :: Maybe RobotPart
--   putStrLn (show (cost <$> maybeRobotPart))


printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "Item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
  putStrLn "Enter a RobotPart part number:"
  userInput <- getLine
  let maybeRobotPart = Map.lookup (read userInput) partsDB

  printCost (cost <$> maybeRobotPart)


