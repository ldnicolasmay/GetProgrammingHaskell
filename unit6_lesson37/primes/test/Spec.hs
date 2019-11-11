-- Spec.hs

import           Data.Maybe
import           Primes
import           Test.QuickCheck

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = do
  putStrLn "" -- Make test stdout more readable
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_factorsMakeOriginal
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_allFactorsPrime


prop_validPrimesOnly n = if n < 2 || upperBound < n
                         then result == Nothing
                         else isJust result
  where result = isPrime n

prop_primesArePrime n = if result == Just True
                        then length divisors == 0
                        else True
  where result   = isPrime n
        divisors = filter ((==0) . (n `mod`)) [2 .. (n-1)]

prop_nonPrimesAreComposite n = if result == Just False
                               then length divisors > 0
                               else True
  where result   = isPrime n
        divisors = filter ((==0) . (n `mod`)) [2 .. (n-1)]

prop_factorsMakeOriginal n = if result == Nothing
                             then True
                             else product (fromJust result) == n
  where result = primeFactors n

prop_allFactorsPrime n = if result == Nothing
                         then True
                         else all (==Just True) resultsPrime
  where result       = primeFactors n
        resultsPrime = map isPrime (fromJust result)

