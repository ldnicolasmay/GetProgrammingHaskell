-- unit5_lesson28_user.hs

data User = User { name    :: String
                 , gamerId :: Int
                 , score   :: Int
                 } deriving Show

-- serverUsername :: Maybe String
-- serverUsername = Just "Sue"

-- serverGamerId :: Maybe Int
-- serverGamerId = Just 1337

-- serverScore :: Maybe Int
-- serverScore = Just 9001

-- userSue :: Maybe User
-- userSue = User <$> serverUsername <*> serverGamerId <*> serverScore


readInt :: IO Int
readInt = read <$> getLine

readString :: IO String
readString = getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamerId and score:"
  user <- User <$> readString <*> readInt <*> readInt
  print user


