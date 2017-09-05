module ParMonad.Futures where

import Control.DeepSeq
import Control.Monad.Par

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor  n 2
                in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m          = n
               | n `mod` m == 0  = m
               | otherwise       = findFactor n ( m + 1 )


findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y  = runPar $ do factorsXVars <- spawnP $ findFactors x
                                  let factorsY = findFactors y
                                      _        = rnf factorsY
                                  factorsX  <- get factorsXVars
                                  return (factorsX, factorsY)
                                      
