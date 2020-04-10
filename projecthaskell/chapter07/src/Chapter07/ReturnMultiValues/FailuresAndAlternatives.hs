module Chapter07.ReturnMultiValues.FailuesAndAlternatives where

import Control.Monad

-- lists represent multiple successful values
-- function represents a machine that can jump randomly
-- one year backward or forward
broken1 :: Integer -> [Integer]
broken1 n = [n-1, n+1]

-- function represents a jump that can jump
-- between 1024 and 2 years in to the future. 
broken2 :: Integer -> [Integer]
broken2 n = [1024, n+2]

-- To form a disjunctive behavior of travelling
-- either by borken1 or broken2
eitherBroken1or2 = broken1 73 `mplus` broken2 73

nothingMPlusJust5 = Nothing `mplus` Just 5

justBroken2 = [1] `mplus` broken2 73

-- Exercise 7-2
find_ ::  (a -> Bool) -> [a] -> Maybe a
find_     _              []     = Nothing
find_     pred           (a:as) =
  if pred a
    then Just a
    else Nothing `mplus` find_ pred as


