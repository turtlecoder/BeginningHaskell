module FirstPrinciples.Chapter14.Addition  where

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n-d) d (count+1) 
         

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy a b = let
  recAdd count accum = if count==0
                       then accum
                       else recAdd (count-1) accum+a
  in
  recAdd b 0
