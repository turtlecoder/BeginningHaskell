{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}


module Chapter03.ParamPoly where

import Data.List hiding (map, foldl, foldr)
import Chapter03.DataTypes
import Prelude hiding (map, ($), duplicate, foldl, foldr, elem)
import Data.Function hiding (($))

maybeString (Just _ ) = "Just"
maybeString Nothing = "Nothing"

-- >>> :t maybeString
-- maybeString :: Maybe a -> [Char]

-- >>> :t head
-- head :: [a] -> a

-- >>> :t fst
-- fst :: (a, b) -> a

-- Excercise 3-1
{-|
Type Info Guess: swapTriple::(a,b,c) -> (b, c, a)

Actual:
-- >>> :t swapTriple
-- swapTriple :: (c, a, b) -> (a, b, c)

-}
swapTriple (x,y,z) = (y,z,x)


{-|
Type Info Guess: duplicate:: a -> (a,a)

Actual:
-- >>> :t duplicate
-- duplicate :: a -> (a, a)
-}
duplicate x = (x,x)

{-|
Type Info Guess: nothing :: a -> Maybe a
Actual:
-- >>> :t nothing
-- nothing :: p -> Maybe a
-}
nothing _ = Nothing

{-|
Type Info Guess: index::[a] -> [(Int, a)]

Actual:
-- >>> :t index
-- index :: Num a => [b] -> [(a, b)]
-}
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in (n+1, x):indexed

{-|
Type Info Guess: maybeA::[a] -> Char

Actual:
-- >>> :t maybeA
-- maybeA :: [a] -> Char
-}
maybeA :: [a] -> Char
maybeA [] = 'a'

map _ [] = []
map f (x:xs) = f x : map f xs

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x+2)

($) :: (a->b) -> a -> b
f $ a = f a

sayHello :: [String]-> [String]
sayHello names = map (\name -> case name of
                         "Alejandro" -> "Hello, Writer"
                         _           -> "Welcomem, " ++ name
                     ) names
                  
sayHello2 :: [String] -> [String]
sayHello2 names = map (\case "Alejandro" -> "Hello, Writer!"                             
                             name        -> "Welcome, " ++ name
                      ) names

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = \x -> n * x

{- Excercise 3-2 -}

filterOnes :: [Integer] -> [Integer]
filterOnes il = filter (\x -> x==1) il

filterANumber :: Integer -> [Integer] -> [Integer]
filterANumber a nl = filter (\x -> x == a) nl

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f al = filter (\a -> not (f a)) al

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs cl = filter isGovOrg cl
  where isGovOrg = (\c -> case c of
                       GovOrg _ _ -> True
                       _        -> False
                   )
filterGovOrgs2 :: [Client a] -> [Client a]
filterGovOrgs2 cl = filter (\case (GovOrg _ _) -> True
                                  _            -> False) cl

duplicateOddsCompose = map (*2) . (filter odd)

duplicateOddsApplicative list = map (*2) $ (filter odd list)


(***) :: (a -> b) -> (c -> d) -> ((a,c)->(b,d))
f *** g = \(x,y) -> (f x , g y)

formula1 :: Integer -> Integer
formula1 = uncurry (+) . (((*7) . (+2)) *** (*3)). duplicate

{- For something like this, should use partition -}
bothFilters :: (a->Bool) -> [a] -> ([a], [a])
bothFilters p list = (filter p list, filter (not.p) list)

skipUntilGov :: [Client a] -> [Client a]
skipUntilGov = dropWhile (\case { GovOrg {} -> False ; _ -> True })

isIndividual :: Client a -> Bool
isIndividual (Individual {} ) = True
isIndividual _                = False

{- Excercise 3-3 -}
product :: [Integer]->Integer
product lst = foldr (*) 1 lst

productPM :: [Integer] -> Integer
productPM lst = case lst of
  (hd:tl) -> hd * (productPM tl)
  []      -> 1

all :: [Bool] -> Bool
all lst = foldr (&&) True lst

allPM :: [Bool] -> Bool
allPM lst = case lst of
  (hd:tl) -> hd && allPM tl
  []      -> True

minimumClient::[Client i] -> Maybe (Client i)
minimumClient cl = foldr cmp Nothing cl
  where
    {- Extracts the name of the client -}
    name = \cl -> case cl of
      Individual { person } -> (firstName person) ++ " " ++ (lastName person)
      _ -> clientName cl
    {- compares the client names -}
    cmp = \cl1 ocl2 -> case ocl2 of
      Nothing -> Just cl1
      Just cl2 -> (if (lenname cl1) < (lenname cl2) then Just cl1 else Just cl2)
        where lenname = length . name

{- Folds Left: -} 
foldl :: (a->b->a) -> a -> [b] -> a
foldl _ initial [] = initial
foldl f initial (x:xs) = foldl f (f initial x) xs
  
{- folds Right: -}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f initial [] = initial
foldr f initial (x:xs) = f x (foldr f initial xs)

isIndividual2 :: Client a -> Bool
isIndividual2 (Individual {}) = True
isIndividual2 _               = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics cs = (any isIndividual cs, not $ (Prelude.all) isIndividual cs)

compareClient :: Client a -> Client b -> Ordering
compareClient (Individual { person = p1 }) (Individual { person = p2 }) = compare (catnames p1) (catnames p2)
  where
    catnames = \Person { firstName = fn, lastName = ln } -> fn ++ " " ++ ln 
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2 = compare (clientName c1) (clientName c2)

elem :: (Eq a) => a -> [a] -> Bool
elem a al = case (find (\x -> x==a) al) of
  Nothing -> False
  Just _  -> True

listOfClients = [ Individual { clientID=2 , person=(Person "H. G." "Wells")}
                , GovOrg 3 "NTTF" -- National Time Travel Foundation
                , Company 4 "Wormhole Inc." (Person "Karl" "Schwarzchild") "Physicist"
                , Individual 5 (Person "Doctor" "")
                , Individual 6 (Person "Sarah" "Jane")
                ]

companyDutiesAnalytics :: [Client a] -> [String]
companyDutiesAnalytics = map (duty . head) .
  sortBy (\x y -> compare (length y) (length x)) .
  groupBy (\x y -> duty x == duty y) .
  filter isCompany
  where isCompany (Company {}) = True
        isCompany _            = False

companyDutiesAnalyticsPF :: [Client a] -> [String]
companyDutiesAnalyticsPF = map (duty.head) .
  sortBy (flip (compare `on` length)) .
  groupBy ( (==) `on` duty) .
  filter isCompany
  where
    isCompany (Company {}) = True
    isCompany _            = False

enum :: Int -> Int -> [Int]
enum a b | a > b = []
enum a b         = a : enum (a+1) b

withPositions :: [a] -> [(Int, a)]
withPositions list = zip (enum 1 $ length list) list

withPositions2 :: [a] -> [(Int, a)]
withPositions2 list = zip [1 .. length list] list

capitals = [("France", "Paris")
           , ("Spain", "Madrid")
           , ("Portugal", "Lisbon")
           ]
