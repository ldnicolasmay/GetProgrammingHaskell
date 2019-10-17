-- unit4_lesson22_sum_lazy.hs


import Data.List.Split (splitOn)


-- main :: IO ()
-- main = do
--   userInput <- getContents
--   -- userInput <- getLine
--   mapM_ print userInput
--   -- print userInput

-- main :: IO ()
-- main = do
--   userInput <- getContents
--   let reversed = reverse userInput
--   putStrLn reversed

-- sampleData = ['6','2','\n','2','1','\n']

-- myLines = splitOn "\n"

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  input <- getContents
  let ints = toInts input
  -- mapM_ print ints
  -- print (sum ints)
  let squares = map (^2) ints
  print (sum squares)