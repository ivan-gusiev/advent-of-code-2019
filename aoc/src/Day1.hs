module Day1
    ( calculateFuel,
      calculateAll,
      readInts
    ) where

{- 
Fuel required to launch a given module is based on its mass. 
Specifically, to find the fuel required for a module, take its mass, 
divide by three, round down, and subtract 2.
-}
calculateFuel :: Integer -> Integer
calculateFuel 0 = 0
calculateFuel mass = fuelFor + calculateFuel fuelFor
    where fuelFor = max 0 (mass `div` 3 - 2)

calculateAll :: [Integer] -> Integer
calculateAll = sum . map calculateFuel

readInts :: String -> IO [Integer]
readInts filename = do
    content <- readFile filename
    let lns = lines content
    return $ map read lns

solve :: String -> IO Integer
solve x = calculateAll <$> readInts x