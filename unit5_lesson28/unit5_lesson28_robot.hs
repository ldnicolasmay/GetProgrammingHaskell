-- unit5_lesson28_robot.hs


import qualified Data.Map as Map


data RobotPart = RobotPart { name        :: String
                           , description :: String
                           , cost        :: Double
                           , count       :: Int
                           } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , description = "arm, left"
                    , cost = 123.45
                    , count = 43
                    }

rightArm :: RobotPart
rightArm = RobotPart { name = "right arm"
                     , description = "arm, right"
                     , cost = 234.56
                     , count = 32
                     }

leftLeg :: RobotPart
leftLeg = RobotPart { name = "left leg"
                    , description = "leg, left"
                    , cost = 321.09
                    , count = 67
                    }

rightLeg :: RobotPart
rightLeg = RobotPart { name = "right leg"
                     , description = "leg, right"
                     , cost = 432.98
                     , count = 2
                     }

torso :: RobotPart
torso = RobotPart { name = "torso"
                  , description = "torso"
                  , cost = 1.12
                  , count = 8273
                  }

partsZip :: [(Int,RobotPart)]
partsZip = zip [1..] [leftArm,rightArm,leftLeg,rightLeg,torso]

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList partsZip

minCostPart :: RobotPart -> RobotPart -> RobotPart
minCostPart part1 part2 = case compare (cost part1) (cost part2) of
                            LT -> part1
                            EQ -> part1
                            GT -> part2

readInt :: IO Int
readInt = read <$> getLine

printPart :: Maybe RobotPart -> IO ()
printPart Nothing     = putStrLn "Error: Invalid part number"
printPart (Just part) = putStrLn (show part)

main :: IO ()
main = do
  --
  putStrLn "Enter first RobotPart part number:"
  partNum1 <- readInt
  let maybePart1 = Map.lookup partNum1 partsDB
  --
  putStrLn "Enter second RobotPart part number:"
  partNum2 <- readInt
  let maybePart2 = Map.lookup partNum2 partsDB
  --
  let cheapPart = minCostPart <$> maybePart1 <*> maybePart2
  printPart cheapPart




