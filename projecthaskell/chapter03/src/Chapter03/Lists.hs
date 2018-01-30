{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Chapter03.Lists where

import Prelude hiding (product)
import Chapter03.DataTypes

-- Excercise 3-3

productP::Num a => [a]->a
productP al = product al 1
  where
    product [] accum = accum
    product (a:al) accum = product al (accum*a)

productF al = foldr (\a b -> a*b) 1 al

minimumClientP :: [(Client i)] -> Maybe (Client i)
minimumClientP cil = minc cil (head' cil)
  where
    minc :: [(Client i)] -> Maybe (Client i) -> Maybe (Client i)
    minc [] cur = cur
    minc (hd:tl) (Just jc) = if (length (name hd) < length (name jc))
                             then minc tl (Just hd)
                             else minc tl (Just jc)
    minc _ Nothing = Nothing

name :: (Client i) -> [Char]
name (Individual _ p) = firstName p ++ lastName p
name c = clientName c

head' [] = Nothing
head' (a:_) = Just a

 
    
minimumClientF :: [(Client i)] -> Maybe (Client i)
minimumClientF cil = foldl (\jc hd -> case jc of 
                                        (Just curr) -> if (length (name  hd) < length (name curr))
                                                       then (Just hd)
                                                       else (Just curr)
                                        Nothing -> Nothing) (head' cil) cil
                     
                     
                               

allP :: [Bool]->Bool
allP bl = let
  allP' [] res = res
  allP' (hd:tl) res = allP' tl (hd && res)
  in
  allP' bl True

allF :: [Bool]-> Bool
allF al = foldl (\b a -> b && a) True al
