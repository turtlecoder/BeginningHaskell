module Chapter08.ParMonads.Futures where

import Control.DeepSeq
import Control.Monad.Par


findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                in oneFactor : (findFactors $ n `div` oneFactor)
                  
findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m = n
               | n `mod` m == 0  = m
               | otherwise = findFactor n (m+1)

findTwoFactorsS :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactorsS x y = (findFactors x, findFactors y)

findTwoFactorsF :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactorsF x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _ = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)


findTwoFactors2 :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors2 x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  factorsYVar <- spawnP $ findFactors y
  factorsX <- get factorsXVar
  factorsY <- get factorsYVar
  return (factorsX, factorsY)
