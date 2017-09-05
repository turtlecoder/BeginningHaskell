module Main(main) where

import ParMonad.Futures

main :: IO ()
main = let
  _ = findTwoFactors 10 20
  in
    putStrLn "Hello, World!"
