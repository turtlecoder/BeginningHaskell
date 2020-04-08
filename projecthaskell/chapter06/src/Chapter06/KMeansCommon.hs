{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Chapter06.KMeansCommon where

import Data.Default

class (Default v, Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v 


instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  -- Calculates the average of list of 2D vectors
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0.0, 0.0) lst
                     n = fromIntegral $ length lst
                 in (u/n, v/n)

-- Converts a data type to a vector provided there is a valid instance
class Vector v => Vectorizable e v where
  toVector :: e -> v


instance Vectorizable (Double,Double) (Double, Double) where
  -- since both are the same types
  toVector = id


type VectorInitFunc e v = Int -> [e] -> [v]

-- >>> :t id
-- id :: a -> a

-- >>> :t centroid
-- centroid :: Vector v => [v] -> v

-- >>> :t (centroid. map toVector)
-- (centroid. map toVector) :: Vectorizable e c => [e] -> c
