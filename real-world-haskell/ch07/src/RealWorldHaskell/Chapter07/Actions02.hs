module RealWorldHaskell.Chapter07.Actions02 where

str2Message :: String -> String
str2Message input = "Data: " ++ input

str2Action :: String -> IO ()
str2Action = putStrLn . str2Message

numbers :: [Int]
numbers = [1..10]

main = do str2Action "Start of the program"
          mapM_ (str2Action.show) numbers
          str2Action "Done!"

          

