module ReturnMoreValues.FailuresAndAlternatives where

import Control.Monad

broken1 :: Integer -> [Integer]
broken1 n = [n-1, n+1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n+2]

broken1_broken2 = broken1 71 `mplus` broken2 72


x1 = Nothing `mplus` Just 5

x2 = Just "first" `mplus` Just "second"


x3 = msum [[1], [2,3], [], [1,2]]


x4 = msum [Nothing, Just 1, Just 2, Nothing]


-- Excercise 7-2 Searching with MonadPlus

find_ :: (a->Bool) -> [a] -> Maybe a
find_ f ls = msum $ (fmap (\a -> if f a then Just a else Nothing)) ls
