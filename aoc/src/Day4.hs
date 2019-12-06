{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (
    isSixDigit, hasAdjacentDigits, digitsIncrease, atLeastOneDouble
  , findRightPasswords
  ) where

    import Data.List(group)

    digits :: Int -> [Int]
    digits x = read . (: []) <$> show x

    pairs :: Int -> [(Int, Int)]
    pairs x = zip ds (tail ds) where ds = digits x

    isSixDigit :: Int -> Bool
    isSixDigit = ((==6) . length) . show

    hasAdjacentDigits :: Int -> Bool
    hasAdjacentDigits = any (uncurry (==)) . pairs
    
    digitsIncrease :: Int -> Bool
    digitsIncrease = all (uncurry (<=)) . pairs

    atLeastOneDouble :: Int -> Bool
    atLeastOneDouble = any ((==2) . length) . group . digits

    findRightPasswords :: Int -> Int -> [Int]
    findRightPasswords lo hi =
        [ i | i <- [lo..hi], isSixDigit i, digitsIncrease i, 
                    hasAdjacentDigits i, atLeastOneDouble i ]