{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter02.SimpleFunctions where

import Chapter02.DataTypes
import Data.Char

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) lst1 lst2 = if null lst1 {- check emptyness -}
                  then lst2 -- base case
                       else (head lst1) : ((tail lst1) +++ lst2)

maxmin lst = if null (tail lst)
             then (head lst, head lst)
             else ( if (head lst) > fst (maxmin (tail lst))
                    then head lst
                    else fst (maxmin (tail lst))
                  , if (head lst) < snd (maxmin (tail lst))
                    then head lst
                    else snd (maxmin (tail lst)))


maxminV2 lst = let h = head lst
           in if null (tail lst)
              then (h, h)
              else ( if h > t_max then h else t_max
                   , if h < t_min then h else t_min)
  where t = maxmin (tail lst)
        t_max = fst t
        t_min = snd t

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _ -> name
  Individual person _ -> case person of Person fName lName gender -> fName ++ " " ++ lName


clientName2 (GovOrg name) = name
clientName2 (Company name _ _ _ ) = name
clientName2 (Individual (Person fName lName _) _) = fName ++ " " ++ lName

fibonacii 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci (n-2)

countGendersHelper :: [Client]->Int->Int -> (Int, Int)
countGendersHelper [] m f = (m,f)
countGendersHelper ((Individual (Person _ _ Male) _):tl) m f = countGendersHelper tl (m+1) f
countGendersHelper ((Individual (Person _ _ Female) _):tl) m f = countGendersHelper tl m (f+1)
countGendersHelper (_:tl) m f = countGendersHelper tl m f 


countGenders::[Client]->(Int, Int)
countGenders cl = countGendersHelper cl 0 0


sorted [] = True
sorted [_] = True
sorted (x : r@(y:_)) = (x<y) && sorted r


ifibonacci :: Integer -> Maybe Integer
ifibonacci n = if n < 0
              then Nothing
              else
                case n of
                  0 -> Just 0
                  1 -> Just 1
                  n' -> let Just f1 = ifibonacci (n'-1)
                            Just f2 = ifibonacci (n'-2)
                        in Just (f1 + f2)


ifibonacciUsingGds n | n < 0 = Nothing
ifibonacciUsingGds 0         = Just 0
ifibonacciUsingGds 1         = Just 1
ifibonacciUsingGds n | otherwise = let (Just f1, Just f2) = (ifibonacciUsingGds (n-1)
                                                            , ifibonacciUsingGds (n-2))
                                   in Just (f1 + f2)

binom _ 0        = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is a beautiful numebr"

ackermann :: Integer -> Integer -> Integer
ackermann m n
  | m == 0 = n+1
  | m > n && n == 0 = ackermann (m-1) 1
  | m > 0 && n > 0  = ackermann (m-1) (ackermann m (n-1))


unzipHelper :: [(a,b)] -> ([a],[b]) -> ([a], [b])
unzipHelper [] accum = accum
unzipHelper ((a,b):rest) (al,bl) = unzipHelper rest (al ++ [a], bl ++ [b])

unzipSF :: [(a,b)] -> ([a],[b])
unzipSF tl = unzipHelper tl ([], [])

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                = "Unknown"


specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _ = False

greet :: ClientR -> String
greet IndividualR { personR = PersonR { firstRName = fn } } = "Hi, " ++ fn
greet CompanyR { clientRName = c } = "Hello, " ++ c
greet GovOrgR {}  = "Welcome"

greetPuns :: ClientR -> String
greetPuns IndividualR { personR = PersonR { firstRName } } = "Hi " ++ firstRName
greetPuns CompanyR { clientRName }                         = "Hello, " ++ clientRName
greetPuns GovOrgR  { }                                     = "Welcome"

greetWildCards IndividualR { personR = PersonR {..} } = "Hi, " ++ firstRName
greetWildCards CompanyR    { .. }                     = "Hello, " ++ clientRName
greetWildCards GovOrgR     { }                        = "Welcome"


nameInCapitals :: PersonR -> PersonR
nameInCapitals p @ (PersonR { firstRName = initial:rest}) =
  let newName = (toUpper initial):rest
  in p { firstRName = newName }
nameInCapitals p @ (PersonR { firstRName = "" }) = p
                                                                   
discountPriceHelper :: [TimeMachine]->Double->[TimeMachine]->[TimeMachine]
discountPriceHelper [] _ accum = accum
discountPriceHelper ((TimeMachine manf model name spec (Price p)):tl) disc accum =
  discountPriceHelper tl disc ((TimeMachine manf model name spec (Price (p*(1.0-disc)))):accum)

discountTM :: [TimeMachine]->Double->[TimeMachine]
discountTM tmLst disc = discountPriceHelper tmLst disc []

updateTMRPrice :: TimeMachineR -> Double -> TimeMachineR
updateTMRPrice (tm @ TimeMachineR { price }) np = tm { price = np }

discountTMRPriceRHelper :: [TimeMachineR]->Double->[TimeMachineR]->[TimeMachineR]
discountTMRPriceRHelper [] _ accum = accum
discountTMRPriceRHelper ((tm @ (TimeMachineR { price = p })):tl) disc accum =
  discountTMRPriceRHelper tl disc (accum ++ [tm { price = (p * (1-disc))}])


discountTMRPrice tmlst disc = discountTMRPriceRHelper tmlst disc []
