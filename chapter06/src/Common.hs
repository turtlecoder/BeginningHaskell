{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Common where

class (Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a) * (c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0.0, 0.0) lst
                     n = fromIntegral $ length lst
                 in
                   (u/n, v/n)
instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

