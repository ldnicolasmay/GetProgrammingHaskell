-- unit3_lesson21_pizza.hs


stringToDouble :: String -> Double
stringToDouble = read

calcPizzaArea :: (Floating a) => a -> a
calcPizzaArea diam = pi * (diam / 2) ^ 2

calcCostPerInch :: (Floating a) => a -> a -> a
calcCostPerInch cost area = cost / area


main :: IO ()
main = do
  putStrLn "What is the size of pizza 1?"
  pizza1SizeStr <- getLine
  putStrLn "What is the cost of pizza 1?"
  pizza1CostStr <- getLine
  putStrLn "What is the size of pizza 2?"
  pizza2SizeStr <- getLine
  putStrLn "What is the cose of pizza 2?"
  pizza2CostStr <- getLine

  let pizza1Diam = stringToDouble pizza1SizeStr
  let pizza2Diam = stringToDouble pizza2SizeStr

  let pizza1Area = calcPizzaArea pizza1Diam
  let pizza2Area = calcPizzaArea pizza2Diam

  let pizza1Cost = stringToDouble pizza1CostStr
  let pizza2Cost = stringToDouble pizza2CostStr

  let pizza1CostPerInch = calcCostPerInch pizza1Cost pizza1Area
  let pizza2CostPerInch = calcCostPerInch pizza2Cost pizza2Area

  let cheaperPizzaSize = if pizza1CostPerInch <= pizza2CostPerInch
                         then pizza1Diam
                         else pizza2Diam
  
  let cheaperPizzaCostPerInch = if pizza1CostPerInch <= pizza2CostPerInch
                                then pizza1CostPerInch
                                else pizza2CostPerInch
  
  putStrLn ("The " ++ show cheaperPizzaSize ++ " pizza is cheaper at " ++ 
            show cheaperPizzaCostPerInch ++ " per square inch.")