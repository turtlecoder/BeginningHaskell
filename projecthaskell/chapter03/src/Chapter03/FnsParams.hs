{-# LANGUAGE LambdaCase #-}

module Chapter03.FnsParams where

import Prelude hiding (map)
import Chapter03.DataTypes


x = succ 1

map _ [] = []
map f (x:xs) = (f x):(map f xs)

xn = map succ [1,2,3]

apply3f2 :: (Integer->Integer) -> Integer -> Integer
apply3f2 f x = 3 * (f x + 2)


m2 = maximum (map succ [1,2,3])

m3 = maximum $ map succ [1,2,3]

-- m2 and m3 are the same

-- Excercise 3-2


filterOnes :: [Integer] -> [Integer]
filterOnes il = filter (\x -> x==1) il

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber a nl = filter (\x -> x == a) nl

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f al = filter (\a -> not (f a)) al

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs cl = filter isGovOrg cl
  where isGovOrg = (\c -> case c of
                       GovOrg _ _ -> True
                       _        -> False
                   )
filterGovOrgs2 :: [Client a] -> [Client a]
filterGovOrgs2 cl = filter (\case (GovOrg _ _) -> True
                                  _            -> False) cl
