{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++)::[a]->[a]->[a]
lst1 +++ lst2 = if null lst1 {- check emptyness -}
                then lst2
                else (head lst1) : (tail lst1 +++ lst2)

-- base not tail -recursive
reverse2 lst = if null lst then [] else reverse2 (tail lst) ++ [head lst]

maxmin list = if null (tail list)
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list)),
                     if(head list) < snd ( maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list)))

maxmin2 list = let h = head list
               in if null (tail list)
                  then (h, h)
                  else ( if h > t_max then h else t_max,
                         if h < t_min then h else t_min)
                       where t = maxmin2 (tail list)
                             t_max = fst t
                             t_min = snd t

maxmin3 [x] = (x,x)
maxmin3 (x:xs) = ( if x > xs_max then x else xs_max ,
                   if x < xs_min then x else xs_min )
  where (xs_max, xs_min) = maxmin xs

data Client = GovOrg String
              | Company String Integer Person String
              | Individual Person Bool
              deriving Show

data Person = Person String String Gender deriving Show

data Gender = Male | Female | Unknown deriving Show

clientName::Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name id person resp -> name
  Individual person ads ->
    case person of Person fname lname gender -> fname ++ " " ++ lname

companyName client = case client of
  Company name _ _ _ -> Just name
  _                  -> Nothing

fibonacci :: Integer -> Integer
fibonacci n = case n of
  0 -> 0
  1 -> 1
  _ -> fibonacci (n-1) + fibonacci (n-2)

f::Client -> String
f client = case client of
  Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
  _                                    -> "There is no boss"

g::Client -> String
g client = case client of
  Company _ _ (Person name _ _ ) pos ->
    case pos of "Boss" -> name ++ " is the boss "
  _                                  -> "There is no boss"


data GenderCounts = GenderCounts Integer Integer Integer
                    deriving Show


countByGenderHelper::[Client]->GenderCounts->GenderCounts
countByGenderHelper clientList (GenderCounts males females unknowns) = case clientList of
  (Individual (Person _ _ Male) _):rest -> countByGenderHelper rest (GenderCounts (males+1) females unknowns)
  (Individual (Person _ _ Female) _):rest -> countByGenderHelper rest (GenderCounts males (females+1) unknowns)
  (Individual (Person _ _ Unknown) _):rest -> countByGenderHelper rest (GenderCounts males females (unknowns + 1))
  _ : rest -> countByGenderHelper rest (GenderCounts males females unknowns)
  []        -> (GenderCounts males females unknowns)

countByGender::[Client]->GenderCounts
countByGender clientList = countByGenderHelper clientList (GenderCounts 0 0 0)

data Manufacturer = Manufacturer Name
                    deriving Show
data Model = Model Integer
             deriving Show 
data Name = Name String
            deriving Show
data TravelDirection = Future
                     | Past
                       deriving Show
data Price = Price Double
             deriving Show
data TimeMachineSpecs = TimeMachineSpecs Name TravelDirection Price
                        deriving Show
                                 
data TimeMachine = TimeMachine Manufacturer Model TimeMachineSpecs
                   deriving Show

data ManufacturerR = ManufacturerR { nameM :: String }
                     deriving Show


data TimeMachineR = TimeMachineR { manufacturer :: ManufacturerR,
                                   model :: ModelR,
                                   specs :: TimeMachineSpecsR
                                 }
                  deriving Show


data ModelR = ModelR { num :: Integer }
            deriving Show

data TimeMachineSpecsR = TimeMachineSpecsR { nameTM :: String ,
                                             dir :: TravelDirection,
                                             price :: Price
                                           }
                         deriving Show
                           
                      

getDiscount :: [TimeMachine] -> Double -> [TimeMachine]
getDiscount timeMachines discount =
  case timeMachines of
    (TimeMachine manf model (TimeMachineSpecs name td (Price p))):rest ->
      (TimeMachine manf model (TimeMachineSpecs name td (Price (p*(1-discount))))):(getDiscount rest discount)
    [] -> []


(+//+) :: [a] -> [a] -> [a]
list1 +//+ list2 = case list1 of
  [] -> list2
  x:xs -> x:(xs +//+ list2)

sorted :: [Integer] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x < y && sorted(y:zs)

sorted2:: [Integer] -> Bool
sorted2 [] = True
sorted2 [_] = True
sorted2 (x: r @ (y:_)) = x < y && (sorted2 r)

ifibonacci n | n < 0 = Nothing
ifibonacci 0         = Just 0
ifibonacci 1         = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)

binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n - 1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y ) == 0

specialMultiples :: Integer -> String
specialMultiples n | multipleOf n 2 = show n ++ " is a multiple of 2 "
specialMultiples n | multipleOf n 3 = show n ++ " is a multiple of 3 "
specialMultiples n | multipleOf n 5 = show n ++ " is a multiple of 5 "
specialMultiples n | otherwise      = show n ++ " is a beautiful number "

ack m n | m==0        = (n+1)
ack m n | m>0 && n==0 = ack (m-1) n
ack m n | m>0 && n>0  = ack (m-1) (ack m (n-1))

data ClientR = GovOrgR { clientRName :: String
                       }
             | CompanyR { clientRName :: String
                        , companyID :: String
                        , person :: PersonR
                        , duty :: String
                        }
             | IndividualR { person :: PersonR
                           }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName :: String
                       }
  deriving Show


greet :: ClientR -> String
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn
greet CompanyR { clientRName = c }                        = "Hello, " ++ c
greet GovOrgR { }                                         = "Welcome"


greet2 IndividualR { person = PersonR { firstName }} = "Hi, " ++ firstName
greet2 CompanyR    { clientRName } = "Hello, " ++ clientRName
greet2 GovOrgR     { }             = "Welcome" 
                            
                            
                 
