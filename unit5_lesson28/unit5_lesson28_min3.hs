-- unit5_lesson28_min3.hs

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree a b c = min a (min b c)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "Enter three integers:"
  minInt <- minOfInts
  putStrLn (show minInt ++ " is the smallest")

