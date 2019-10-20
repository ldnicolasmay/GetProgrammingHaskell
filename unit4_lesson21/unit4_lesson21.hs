-- unit3_lesson21.hs

import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   putStrLn "What's your name?"
--   name <- getLine
--   let statement = helloPerson name
--   putStrLn statement


-- --
-- [1,2,3] >>= \x -> [4,5,6] >>= \y -> return (x+y)
-- Just 3 >>= \x -> Just 4 >>= \y -> return (x+y)
-- main :: IO ()
-- main = do
--   let x = 3
--   let y = 4
--   print (x+y)

-- --
-- helloPersonIO :: IO String -> IO String
-- helloPersonIO nameIO = nameIO >>= \name -> return ("Hello, " ++ name ++ "!")
  
-- --
-- helloPersonDo :: IO String -> IO String
-- helloPersonDo nameIO = do
--   name <- nameIO
--   return ("Hello, " ++ name ++ "!")

-- --
-- main :: IO ()
-- main = do
--   putStrLn "What's your name?"
--   -- let nameIO = getLine
--   -- statement <- helloPersonIO nameIO
--   statement <- helloPersonIO getLine
--   -- statement <- helloPersonDo nameIO
--   putStrLn statement


--
nameData :: Map.Map Int String
nameData = Map.fromList [(1,"Nic")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 nameData
  let statement = helloPerson name
  return statement