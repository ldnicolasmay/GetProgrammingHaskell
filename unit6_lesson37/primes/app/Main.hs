-- Main.hs

module Main where

-- import Lib
import           Data.Maybe
import           Primes

main :: IO ()
-- main = return ()
main = do
  -- putStrLn "Enter a number to check if it's prime:"
  -- input <- getLine
  -- let num = read input :: Int
  -- let response = if fromJust (isPrime num)
  --                then "It is prime!"
  --                else "It isn't prime."
  -- putStrLn response
  --
  putStrLn "For an integer, check its [p]rimality or [f]actor it into primes?"
  menuOpt <- getLine -- 'p' or 'f'
  --
  let prompt = if head menuOpt == 'p'
               then "Enter an integer for primality test:"
               else
                 if head menuOpt == 'f'
                 then "Enter an integer to factor:"
                 else "Invalid option."
  putStrLn prompt
  intStr <- getLine
  let int = read intStr :: Int
  let result = if head menuOpt == 'p'
               then show (fromJust (isPrime int))
               else
                 if head menuOpt == 'f'
                 then show (fromJust (primeFactors int))
                 else "Blah!"
  putStrLn result

