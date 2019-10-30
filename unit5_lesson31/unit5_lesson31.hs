-- unit5_lesson31.hs


maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM mpair = mpair >>= \pair -> (\max -> return max) (maximum pair)
-- maxPairM mpair = mpair >>= \pair -> return (maximum pair)
-- maxPairM mpair = do
--   pair <- mpair
--   let max = maximum pair
--   return max


-- 31.1 do Notation Revisited

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " <> name <> "!"

helloName :: IO ()
helloName = do
  askForName
  name <- getLine
  putStrLn (nameStatement name)


-- main :: IO ()
-- main = helloName

helloPerson :: String -> String
helloPerson name = "Hello, " <> name ++ "!"

-- main :: IO ()
-- main = do
--   name <- getLine
--   let statement = helloPerson name
--   putStrLn statement

main :: IO ()
main = getLine >>= \name -> (\statement -> putStrLn statement) (helloPerson name)
-- main = getLine >>= \name -> (putStrLn (helloPerson name))

