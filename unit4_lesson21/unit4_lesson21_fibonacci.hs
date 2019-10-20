-- unit3_lesson21_fibonacci.hs

fastFib :: (Integral a) => a -> a -> a -> a
fastFib m _ 1 = m
fastFib m n f = fastFib n (m+n) (f-1)

fibN :: (Integral a) => a -> a
fibN = fastFib 0 1

main :: IO ()
main = do
  putStr "Which nth Fibonacci number would you like? "
  nStr <- getLine
  putStrLn . show . fibN $ read nStr