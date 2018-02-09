module Chapter15.BuggyLists where

reverse' :: [a] -> [a]
reverse' [] = []
{- Uncomment the line below to fail the test -}
-- reverse' [x] = [x,x]
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x] 

