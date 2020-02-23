module Chapter05.Infinity where

import Data.List
import Debug.Trace

data TimeMachine = TM { _manufacturer :: String
                      , _year :: Integer } deriving (Eq, Show)


timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

threeTimeMachines = take 3 timelyIncMachines

-- >>> :t repeat
-- repeat :: a -> [a]

-- >>> take 3 $ repeat $ TM "Timely Inc." 2020
-- [TM {_manufacturer = "Timely Inc.", _year = 2020},TM {_manufacturer = "Timely Inc.", _year = 2020},TM {_manufacturer = "Timely Inc.", _year = 2020}]

findMachines = find (\TM { _year = y } -> y > 2018) timelyIncMachines


zipWithNumbers = (\list -> zip [ 1 .. length list] list) "abcde"

-- >>> zipWithNumbers
-- [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]


allNumbers = allNumbersFrom 1

allNumbersFrom n = n : allNumbersFrom (n+1)

anotherZipNumbers = zip allNumbers "abcde"

thirdZipNumbers = zip [ 1 .. ] "abcde"

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- >>> :t zipWith
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]


_20thfibonacci = fibonacci !! 20

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines

take3infinite2020Machines = take 3 $ repeat $ TM "Timely Inc." 2020

specialOffer = cycle [ TM m 2005, TM m 1994, TM m 1998]
  where
    m = "Timely Inc."

-- >>> :t cycle
-- cycle :: [a] -> [a]


take4SpecialOffer = take 4 specialOffer
-- >>> take4SpecialOffer
-- [TM {_manufacturer = "Timely Inc.", _year = 2005},TM {_manufacturer = "Timely Inc.", _year = 1994},TM {_manufacturer = "Timely Inc.", _year = 1998},TM {_manufacturer = "Timely Inc.", _year = 2005}]


fibonacci2 = map fst $ iterate (\(n, n') -> (n', n+n')) (0, 1)

-- >>> :t iterate
-- iterate :: (a -> a) -> a -> [a]

_20thFibonacci2 = fibonacci2 !! 20
-- >>> _20thFibonacci2
-- 6765

-- Excercise 5-1: The sieve of Erosthenese
primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve (p:xs) =  p : sieve (minus xs [p, p + p ..])

minus (x:xs) (y:ys) = 
  case compare x y of
     LT -> x:minus xs (y:ys)
     EQ -> minus xs ys
     GT -> minus (x:xs) ys
               
minus xs _ = xs
                             
-- >>> take 2 primes 
-- [2,3]


-- >>> :type-at Chapter05.Infinity 76 76 76 76 minus
-- minus :: forall a. Ord a => [a] -> [a] -> [a]

-- >>> :type-at Chapter05.Infinity 72 72 72 72 primes
-- primes :: [Integer]


-- >>> 2
-- 2

-- >>> :t fibonacci
-- fibonacci :: [Integer]

-- >>> :t (!!)
-- (!!) :: [a] -> Int -> a

-- >>> (repeat 1 ) !! 10
-- 1

-- >>> :t repea
-- repeat :: a -> [a]


