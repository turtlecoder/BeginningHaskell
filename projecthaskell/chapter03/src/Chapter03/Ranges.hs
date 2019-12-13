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
  (r -> R a b) -> "[" ++ show a ++ "," ++ show b ++ "]"


-- >>> range 1 2  
-- Range 1 2
-- >>> range 2 1 -- *** Exception: a must be <= b
-- CallStack (from HasCallStack):
--   error, called at /var/folders/q1/nr1cwm0x5s7d1q11374kdljw0000gr/T/dante7EFRVu.hs:11:43 in main:Chapter03.Ranges

-- >>> Range 3 2 
-- Range 3 2
