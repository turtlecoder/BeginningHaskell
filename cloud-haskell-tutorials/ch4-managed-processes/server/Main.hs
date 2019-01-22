module Main where

import MathServer

main :: IO ()
main = do
  putStrLn "Hello, World!"
  initMathServer "127.0.0.1" 8080
