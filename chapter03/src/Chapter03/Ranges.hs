{-# LANGUAGE ViewPatterns #-}

module Chapter03.Ranges (Range(), RangeObs(R), range ) where

data Range = Range Integer Integer deriving Show

data RangeObs = R Integer Integer deriving Show

{- Smart Constructor -}
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"


r::Range -> RangeObs
r (Range a b) = R a b


prettyRange:: Range -> String
prettyRange rng = case rng of
  Range a b -> "[" ++ show a ++ "," ++ show b ++ "]"
