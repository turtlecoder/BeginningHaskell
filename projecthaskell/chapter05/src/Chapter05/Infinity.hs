module Chapter04.Infinity where

import Data.List

data TimeMachine = TM { _manufacturer :: String
                      , _year :: Integer } deriving (Eq, Show)


timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y+1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

threeTimeMachines = take 3 timelyIncMachines

findMachines = find (\TM { _year = y } -> y > 2018) timelyIncMachines


zipWithNumbers = (\list -> zip [ 1 .. length list] list) "abcde"


allNumbers = allNumbersFrom 1

allNumbersFrom n = n : allNumbersFrom (n+1)

anotherZipNumbers = zip allNumbers "abcde"

thirdZipNumbers = zip [ 1 .. ] "abcde"

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)


_20thfibonacci = fibonacci !! 20

infinite2020Machines :: [TimeMachine]
infinite2020Machines = TM "Timely Inc." 2020 : infinite2020Machines

take3infinite2020Machines = take 3 $ repeat $ TM "Timely Inc." 2020

specialOffer = cycle [ TM m 2005, TM m 1994, TM m 1998]
  where
    m = "Timely Inc."


take4SpecialOffer = take 4 specialOffer


fibonacci2 = map fst $ iterate (\(n, n') -> (n', n+n')) (0, 1)

_20thFibonacci2 = fibonacci2 !! 20

-- Excercise 5-1: The sieve of Eratosthenese
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve (minus xs [p, p+p ..])
    minus (x:xs) (y:ys) = case compare x y of
                            LT -> x:minus xs (y:ys)
                            EQ -> minus xs ys
                            GT -> minus (x:xs) ys
    minus a _ = a
                             

