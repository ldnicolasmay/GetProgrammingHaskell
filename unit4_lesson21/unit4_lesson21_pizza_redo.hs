-- unit3_lesson21_pizza_redo.hs

type Pizza = (Double,Double)

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if pricePerInch p1 <= pricePerInch p2
                      then p1
                      else p2

pizzaArea :: Double -> Double
pizzaArea size = pi * (size / 2) ^ 2

pricePerInch :: Pizza -> Double
pricePerInch (size,cost) = cost / pizzaArea size

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza is cheaper at " ++
                            show (pricePerInch (size,cost)) ++ " per sq in"

main :: IO ()
main = do
  -- Pizza 1
  putStr "What is the size of pizza 1? "
  p1Size <- getLine
  putStr "What is the cost of pizza 1? "
  p1Cost <- getLine
  -- Pizza 2
  putStr "What is the size of pizza 2? "
  p2Size <- getLine
  putStr "What is the cost of pizza 2? "
  p2Cost <- getLine
  -- 
  let pizza1 = (read p1Size,read p1Cost)
  let pizza2 = (read p2Size,read p2Cost)
  --
  let cheaperPizza = comparePizzas pizza1 pizza2

  putStrLn (describePizza cheaperPizza)