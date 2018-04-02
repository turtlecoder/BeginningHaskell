module Concat where

-- Basically a flatten
concat :: [[a]] -> [a]
concat a = foldr (++) [] a
