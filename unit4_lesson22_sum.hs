-- unit4_lesson22_sum.hs

import System.Environment (getArgs)
import Control.Monad (replicateM)

-- --
-- main :: IO ()
-- main = do
--   args <- getArgs
--   mapM_ putStrLn args

-- --
-- main :: IO ()
-- main = do
--   vals <- mapM (\_ -> getLine) [1 .. 3]
--   mapM_ putStrLn vals

-- --
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let linesToRead = if length args > 0
--                     then read (head args)
--                     else 0 :: Int
--   print linesToRead

--
main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0
  numbers <- replicateM linesToRead getLine
  -- numbers <- mapM (\_ -> getLine) [1 .. linesToRead]
  let numbersInt = map read numbers :: [Int]
  print (sum numbersInt)