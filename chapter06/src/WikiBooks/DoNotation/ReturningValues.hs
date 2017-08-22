module ReturningValues where

nameReturn :: IO String
nameReturn = do putStr "What is your first Name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                putStrLn ("Pleased to meet you, " ++ full ++ "!")
                return full


greetAndSeeYou :: IO ()
greetAndSeeYou = do name <- nameReturn
                    putStrLn ("See You, " ++ name ++ "!")



nameReturnAndCarryOn = do putStr "What your first name? "
                          first <- getLine
                          putStr "And your last name? "
                          last <- getLine
                          let full = first ++ " " ++ last
                          putStrLn $ "Pleased to meet you, " ++ full ++ "!"
                          return full
                          putStrLn "I am not finished yet"

