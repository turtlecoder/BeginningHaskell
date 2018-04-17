module HaskellWiki.Monad.ST where

import Control.Monad.ST
import Data.STRef
import Control.Monad

sumST :: Num a => [a] -> a              -- 
sumST xs = runST $ do n <- newSTRef 0   -- runST takes out stateful code and makes it pure again
                      forM_ xs $ \x -> do     -- for each element of xs .. 
                        modifySTRef n (+x) -- add it to what we have in n 
                      readSTRef n          -- read the value of n and return it

foldlST :: (a->b->a) -> a -> [b] -> a
foldlST f acc xs = runST $ do
  acc' <- newSTRef acc     --- Create a variable for the accumulator 
  forM_ xs $ (\x -> do      -- for each x in xs .. 
    a <- readSTRef acc'    -- apply f to the accumulator and x 
    writeSTRef acc' (f a x))
  readSTRef acc'


fibST :: Integer -> Integer
fibST n =
  if n < 2
  then n
  else runST $ do
    x <- newSTRef 0
    y <- newSTRef 1
    fibST' n x y
      where fibST' 0 x _ = readSTRef x
            fibST' n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y $! x' + y'
              fibST' (n-1) x y 
