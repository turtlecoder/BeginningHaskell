{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common where

class Vector v where
  distance :: v -> v -> Double


class Vector v => Vectorizable e v where
  toVector :: e -> v


