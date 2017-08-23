module ReturnMoreValues.Lists where

x01 = return 1 :: [Integer]

x02 = [1, 2, 3] >>= \x -> [2*x, 3*x]

x03 = map (\x -> [2*x, 3*x]) [1,2,3]

x04 = concat $ map (\x -> [2*x, 3*x]) [1,2,3]

everyCombination = do x <- [1,2,3]
                      y <- [7,8,9]
                      return (x*y)


brokenThreeJumpsHelper year = do y <- [year]
                                 jmps <- [-1, 3, 5]
                                 return $ y + jmps
-- Excercise 7-1 Broken Time Machines

brokenThreeJumps year = do jump1 <- brokenThreeJumpsHelper year
                           jump2 <- brokenThreeJumpsHelper jump1
                           jump3 <- brokenThreeJumpsHelper jump2
                           return $ jump3
                                 
                                     

brokenJumps allowedJumps year n = do y <- [year]
                                     jmp <- allowedJumps
                                     if n == 1
                                       then return $ y+jmp
                                       else brokenJumps allowedJumps (y+jmp) (n-1)
