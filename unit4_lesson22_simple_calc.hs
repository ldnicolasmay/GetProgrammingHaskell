-- unit4_lesson22_simple_calc.hs

import Data.List.Split

-- -- input      = ["1+2\n2+3\n3+4"]
-- -- inputLines = ["1+2","2+3","3+4"]
-- -- numsStr    = [["1","2"],["2","3"],["3","4"]]
-- -- numsInt    = [[1,2],[2,3],[3,4]]
-- -- sumOrProd  = ["sum","sum","sum"]

-- sumOrProdFxn :: String -> String
-- sumOrProdFxn formula
--   | '+' `elem` formula = "sum"
--   | '*' `elem` formula = "product"
--   | otherwise          = "sum"

-- addOrMultiply :: String -> [Int]
-- addOrMultiply input = inputSum
--   where inputLines = lines input
--         sumOrProd  = map sumOrProdFxn inputLines 
--         numsStr    = if last sumOrProd == "sum"
--                      then map (splitOn "+") inputLines
--                      else map (splitOn "*") inputLines
--         numsInt    = map toInts numsStr
--         inputSum   = if last sumOrProd == "sum"
--                      then map sum numsInt
--                      else map product numsInt
        

-- toInts :: [String] -> [Int]
-- toInts = map (\n -> read n :: Int)

-- main :: IO ()
-- main = do
--   input <- getContents                       -- input :: [String]
--   -- let inputLines = lines input               -- inputLines :: [String]
--   -- let numsStr = map (splitOn "+") inputLines -- numsStr :: [[String]]
--   -- let numsInt = map toInts numsStr           -- numsInt :: [[Int]]
--   -- mapM_ (print . sum) numsInt
--   let results = addOrMultiply input
--   mapM_ print results

-- My solution was WAY MORE COMPLICATED than necessary... 
-- The trick is pattern matching... fuuuuuuuuuuuuck :(

sampleInput :: [String]
sampleInput = ["21","+","123"]

calc :: [String] -> Int
calc (num1:"+":num2:_) = read num1 + read num2
calc (num1:"*":num2:_) = read num1 * read num2

-- input      = "1 + 2\n3 * 4\n"
-- inputLines = lines input
-- formulas   = map (splitOn " ") inputLines
-- results    = map calc formulas

main :: IO ()
main = do
  input <- getContents                 -- "1 + 2\n3 * 4\n"
  let inputLines = lines input         -- ["1 + 2","3 * 4"]
  let formulas   = map (splitOn " ") inputLines -- [["1","+","2"],["3","*","4"]]
  let results    = map calc formulas   -- [3,12]
  mapM_ print results
