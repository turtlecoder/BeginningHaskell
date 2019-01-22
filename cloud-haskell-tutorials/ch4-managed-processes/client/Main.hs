module Main where

import MathClient

import MathClient

main :: IO ()
main = do
  putStrLn "Hello, World from the client!!!"
  mathClientAdd ("127.0.0.1", 8080) ("127.0.0.1", 8081) 2.0 4.0
  return ()
