module Chapter08.TheParMonad.Futures where

import Control.DeepSeq
import Control.Monad.Par
-- import Control.Concurrent

-- findFactorsIO :: Integer -> IO [Integer]
-- findFactorsIO 1 = return [1]
-- findFactorsIO n = let oneFactor = findFactor n 2
--                   in do
--   threadDelay 5000000
--   xl <- findFactorsIO (n `div` oneFactor)
--   return $ oneFactor : xl
  

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                    in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise       = findFactor n (m+1)


findTwoFactors :: Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)


