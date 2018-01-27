{-# LANGUAGE ViewPatterns #-}

module Chapter02.PatternMatching where

import Chapter02.Data

-- simple patterns
clientName :: Client -> String
clientName client  = case client of
                       GovOrg name -> name
                       Company name id person resp -> name
                       Individual person ads ->
                         case person of Person fname lname gender -> fname ++ " " ++ lname


clientName2 :: Client -> String
clientName2 client = case client of
                       GovOrg name -> name
                       Company name id person resp -> name
                       Individual (Person fname lname _) _ -> fname ++ " " ++ lname

-- throws an exception, when there is no match
companyName :: Client -> String
companyName client = case client of Company name _ _ _ -> name


companyName2 :: Client -> Maybe String
companyName2 client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n-1) + fibonacci (n-2)

f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss."
             _                                    -> "There is no boss"


-- Causes an exception
g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _ ) pos ->
               case pos of "Boss" -> name ++ " is the boss."
             _                                     -> "There is no boss."


clientName3 (GovOrg name) = name
clientName3 (Company name _ _ _) = name
clientName3 (Individual (Person fname lname _) _) = fname ++ " " ++ lname

fibonacci2 0 = 0
fibonacci2 1 = 1
fibonacci2 n = fibonacci2 (n-1) + fibonacci2 (n-2)

-- Excercise 2-5 Perfect Match for your Time Machines
countGenders::[Client]->(Integer, Integer, Integer)
countGenders [] = (0,0,0)
countGenders (hd:tl) = let (maleCount, femaleCount, otherCount) = countGenders tl
                           countGender' :: Gender -> (Integer, Integer, Integer)
                           countGender' Male = (maleCount+1, femaleCount, otherCount)
                           countGender' Female = (maleCount, femaleCount+1, otherCount)
                           countGender' Other = (maleCount, femaleCount, otherCount)
                       in case hd of
                            GovOrg _ -> (maleCount, femaleCount, otherCount)
                            Company _ _ (Person _ _ gender) _ -> countGender' gender
                            Individual (Person _ _ gender)  _ -> countGender' gender

-- Excercise 2-5
discountTimeMachines :: [TimeMachine] -> Double -> [TimeMachine]
discountTimeMachines [] _ = []
discountTimeMacines ((TimeMachine manf model name dir p):tl) discount  = (TimeMachine manf model name dir (p*discount)): (discountTimeMachines tl discount)                                                                        


--

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x < y && sorted (y:zs)

sorted2 [] = True
sorted2 [_] = True
sorted2 (x: r @ (y:zs)) = x < y && sorted r

maxmin [x] = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min)
                where (xs_max, xs_min) = maxmin xs

--

ifibonacci :: Integer -> Maybe Integer
ifibonacci n = if n < 0
               then Nothing
               else case n of
                      0 -> Just 0
                      1 -> Just 1
                      n' -> let Just f1 = ifibonacci (n'-1)
                                Just f2 = ifibonacci (n'-2)
                            in
                              Just (f1+f2)
--

binom _ 0        = 1
binom x y | x==y = 1
binom n k        = (binom (n-1) (k-1)) + (binom (n-1) k)

--

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0 

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is multiple of 2"
specialMultiples n | multipleOf n 3 = show n ++ " is multiple of 3"
specialMultiples n | multipleOf n 5 = show n ++ " is multiple of 5"
specialMultiples n | otherwise = show n ++ " is a beautiful number"

-- Excercise 2-6

ackermann m n | m==0 = n+1
ackermann m n | m>0 && n==0 = ackermann (m-1) 1
ackermann m n | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))

-- Using View Patterns

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _                              = False
