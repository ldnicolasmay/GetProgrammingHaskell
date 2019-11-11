-- Primes.hs

module Primes
    ( upperBound
    , primes
    , sieve
    , isPrime
    , primeFactors
    ) where

upperBound :: Int
upperBound = 10000

primes :: [Int]
primes = sieve [2 .. upperBound]

sieve :: [Int] -> [Int]
sieve []     = []
sieve (n:ns) = n : sieve filterNs
  where filterNs = filter (\n_ -> n_ `mod` n /= 0) ns

isPrime :: Int -> Maybe Bool
isPrime n | n < 2          = Nothing
          | n > upperBound = Nothing
          | otherwise      = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 1 _  = []
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (p:rimes) = if n `mod` p == 0
                                 then p : unsafePrimeFactors (n `div` p) (p:rimes)
                                 else unsafePrimeFactors n rimes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= upperBound = Nothing
               | otherwise = Just (unsafePrimeFactors n primesLessThanN)
  where primesLessThanN = filter (<=n) primes


