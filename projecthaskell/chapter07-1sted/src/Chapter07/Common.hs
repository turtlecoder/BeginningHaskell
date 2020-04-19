module Chapter07.Common where

graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004) , (1004, 501), (2013, 2558)]

graph1 :: [(Int, Int)]
graph1 = [(2013, 501) , (2013, 1004), (501, 2558), (1004, 2558)]

gx = do x <- [1,2,3]
        y <- [7,8,9]
        return $ x*y

-- >>> :t gx
-- gx :: [Integer]

-- >>> gx
-- [7,8,9,14,16,18,21,24,27]
