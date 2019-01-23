module RealWorldHaskell.Chapter07.BasicIO where

main :: IO ()
main = do
  putStrLn "Greetings! What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
