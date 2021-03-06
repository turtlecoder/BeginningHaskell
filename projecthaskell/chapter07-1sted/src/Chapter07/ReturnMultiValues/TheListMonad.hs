module Chapter07.ReturnMultiValues.TheListMonad where


import Data.List (sort)

listMonad1 = return 1::[Integer]

-- composition of mapping and then flattening
multi2and3 = [1,2,3] >>= \x -> [2*x, 3*x]

-- just mapping

mappingOnly = fmap (\x -> [2*x, 3*x]) [1,2,3]

-- and then flattne

flattenedMapping = concat mappingOnly

-- alternatively

altfalttenedMapping = concat $ fmap (\x -> [2*x, 3*x]) [1,2,3]

-- Using do notation

usingDo = do x <- [1,2,3]
             y <- [7,8,9]
             return $ x * y 

-- src/Chapter07/ReturnMultiValues/TheListMonad.hs
-- Excercise 7-1
-- Good Interview Question!!
brokenThreeJumps :: (Num year) => year -> [year]
brokenThreeJumps year =
  let jumpBwd1 yr = yr - 1
      jumpFwd3 yr = yr + 3
      jumpFwd5 yr = yr + 5
      listFn = [jumpBwd1, jumpFwd3, jumpFwd5]
   in do jumpFn1 <- listFn
         jumpFn2 <- listFn
         jumpFn3 <- listFn
         return $ jumpFn3 $ jumpFn2 $ jumpFn1 year

-- >>> brokenThreeJumps 2020
-- [2017,2021,2023,2021,2025,2027,2023,2027,2029,2021,2025,2027,2025,2029,2031,2027,2031,2033,2023,2027,2029,2027,2031,2033,2029,2033,2035]
         


brokenJumps :: (Show year, Num year, Num jumps, Eq jumps) => year ->jumps -> [year] -> [year]
brokenJumps startYear 0              accumYears = startYear:accumYears
brokenJumps startYear jumpsRemaining accumYears =
  let jmpBwd1 = \yr -> yr - 1
      jmpFwd3 = \yr -> yr + 3 
      jmpFwd5 = \yr -> yr + 5
  in do fn <- [jmpBwd1, jmpFwd3, jmpFwd5]
        let newYear = fn startYear        
        brokenJumps newYear (jumpsRemaining - 1) (accumYears)

-- >>> brokenJumps 2020 1 []
-- [2019,2023,2025]
