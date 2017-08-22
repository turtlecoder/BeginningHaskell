module WikiBooks.DoNotation.UIProg where

nameDo :: IO ()
nameDo = do putStr "What is your first name? " 
            first <- getLine
            putStr "And your last name? "
            last <- getLine
            let full = first ++ " " ++ last
            putStrLn ("Pleased to meet you, " ++ full ++ "!" )

            
nameLambda :: IO ()
nameLambda = putStr "What is your first name? " >>
             getLine >>=
             \first -> putStr "And your last name? " >>
             getLine >>=
             \last ->
               let full = first ++ " " ++ last
               in putStrLn ("Pleased to meet you, " ++ full ++ "!")
                           
