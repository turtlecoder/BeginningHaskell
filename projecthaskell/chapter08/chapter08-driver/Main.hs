
module Main where

import Chapter08.TheParMonad.Futures

main :: IO ()
main = do
  let (a, b) = findTwoFactors 123300 24256
  putStrLn $ show (a,b)
  return ()
  
