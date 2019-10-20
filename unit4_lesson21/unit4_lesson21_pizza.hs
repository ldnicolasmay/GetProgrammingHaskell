-- unit3_lesson21_pizza.hs


areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size,cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 <= costP2
                      then p1
                      else p2
  where costP1 = costPerInch p1
        costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza is cheapter at " ++
                            show costSqInch ++ " per square inch."
  where costSqInch = costPerInch (size,cost)


main :: IO ()
main = do
  -- pizza 1
  putStrLn "What is the size of pizza 1?"
  p1SizeStr <- getLine
  putStrLn "What is the cost of pizza 1?"
  p1CostStr <- getLine
  -- pizza 2
  putStrLn "What is the size of pizza 2?"
  p2SizeStr <- getLine
  putStrLn "What is the cose of pizza 2?"
  p2CostStr <- getLine

  -- create pizzas
  let pizza1 = (read p1SizeStr, read p1CostStr)
  let pizza2 = (read p2SizeStr, read p2CostStr)

  -- compare pizzas
  let cheaperPizza = comparePizzas pizza1 pizza2

  putStrLn (describePizza cheaperPizza)
