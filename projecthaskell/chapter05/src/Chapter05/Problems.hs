module Chapter05.Problems where

-- do not run Out of Memory Exception will be caused
-- because thunking in laziness
outOfMemoryError = foldl (+) 0 [0 .. 100000000000]

sumForce :: [Integer] -> Integer
sumForce xs = sumForce' xs 0
  where
    sumForce' [] z = z 
    sumForce' (y:ys) z = let s = y + z in seq s sumForce' ys s 
