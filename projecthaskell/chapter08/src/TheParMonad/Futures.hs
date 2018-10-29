module TheParMonad.Futures where

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                    in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise       = findFactor n (m+1)
