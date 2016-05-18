module Chapter03.FnsParams where

import Prelude hiding (map)

map _ [] = []
map f (x:xs) = (f x):(map f xs)

