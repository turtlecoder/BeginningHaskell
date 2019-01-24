module RealWorldHaskell.Chapter07.ToUpperLazy05 where

import Data.Char(toUpper)

main = interact (map toUpper . (++) "Your Data, in Upper Case is: \n\n")
