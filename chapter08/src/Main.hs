module Main(main) where

import ParMonad.Futures
import ParMonad.SKMeans

main :: IO ()
main = let
  _ = findTwoFactors 10 20
  in
    putStrLn "Hello, World"
