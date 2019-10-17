-- unit4_lesson22_quotes.hs

-- sampleData :: [Int]
-- sampleData = [1,2,3,4,5]

-- getQuote :: String -> String
-- getQuote numStr
--   | numStr == "1" = "foo"
--   | numStr == "2" = "bar"
--   | numStr == "3" = "baz"
--   | numStr == "4" = "qux"
--   | numStr == "5" = "xyzzy"

-- main :: IO ()
-- main = do
--   input <- getContents
--   let inputLines = lines input
--   let quotes = map getQuote inputLines
--   mapM_ (\q -> putStr (q ++ "\nAnother quote? ")) quotes

quotes :: [String]
quotes = ["quote 1"
         ,"quote 2"
         ,"quote 3"
         ,"quote 4"
         ,"quote 5"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where quote = quotes !! (read x - 1)

main :: IO ()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote  (lines userInput))