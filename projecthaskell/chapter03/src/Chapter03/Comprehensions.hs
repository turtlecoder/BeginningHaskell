{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TransformListComp #-}

module Chapter03.Comprehensions where

import Prelude 

doubleOdds :: [Integer] -> [Integer]
doubleOdds list = map (*2) $ filter odd list

doubleOdds2 list = [ 2 * x | x <- list, odd x]

ex1 = [(x,y, x*y) | x <- [1 .. 4 ], y <- [1..10], odd x, even y]
ex2 = [ sqrt v | (x,y) <- [(1,2), (3,8)], let v = x * x + y * y]

testThenReverseData = [x*y | x <- [-1, 1, -2], y<-[1,2,3], then reverse]
