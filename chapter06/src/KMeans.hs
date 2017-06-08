{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


module VectorDouble where

import Common

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a) * (c-a) + (d-b) * (d-b)

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

kMeans:: ( Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> [e] -> [v]
kMeans initF e = undefined
