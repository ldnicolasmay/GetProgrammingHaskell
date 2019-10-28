-- unit5_lesson30.hs


-- 30.1 The Limitations of Applicative and Functor


import qualified Data.Map as Map


type UserName      = String
type GamerId       = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1,"nYarlathoTep")
                          , (2,"KINGinYELLOW")
                          , (3,"dagon1997")
                          , (4,"rcarter1919")
                          , (5,"xCTHULHUx")
                          , (6,"yogSOThoth")
                          ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlathoTep",2000)
                         , ("KINGinYELLOW",15000)
                         , ("dagon1997",300)
                         , ("rcarter1919",12)
                         , ("xCTHULHUx",50000)
                         , ("yogSOThoth",150000)
                         ]

-- λ> Map.lookup 1 userNameDB >>= \user -> Map.lookup user creditsDB >>= \credit -> return (credit)
-- Just 2000
-- λ> Map.lookup 2 userNameDB >>= \user -> Map.lookup user creditsDB >>= \credit -> return (credit)
-- Just 15000

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId gamerId = do
--   user   <- Map.lookup gamerId userNameDB
--   credit <- Map.lookup user creditsDB
--   return credit


lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB



-- 30.2 The Bind Operator: >>=


creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits


type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [ (1001,1)
                         , (1002,2)
                         , (1003,3)
                         , (1004,4)
                         , (1005,5)
                         , (1006,6)
                         ]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId wcId = lookupGamerId wcId >>= lookupUserName >>= lookupCredits


-- readInt :: IO Int
-- readInt = read <$> getLine

-- printDouble :: Int -> IO ()
-- printDouble n = print (n*2)

-- main :: IO ()
-- main = do
--   readInt >>= printDouble


-- 30.3 The Monad Type Class

-- Definition of Monad
-- class Applicative m => Monad (m :: * -> *) where
--   (>>=)  :: m a -> (a -> m b) -> m b
--   (>>)   :: m a -> m b -> m b
--   return :: a -> m a
--   fail   :: String -> m a


-- Q30.1

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f mx = mx >>= \x -> return (f x)     -- mx :: m a
                                              -- x :: a
                                              -- f x :: b
                                              -- return (f x) :: m b

-- Q30.2

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf mx = mf >>= \f -> mx >>= \x -> return (f x)

-- λ> pure (+) `allApp` (Just 3) `allApp` (Just 4)
-- Just 7

-- Q30.3

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- bind mx f = mx >>= \x -> f x                  -- mx :: Maybe a
--                                               -- x :: a
--                                               -- f x :: Maybe b
bind Nothing _  = Nothing
bind (Just x) f = f x


