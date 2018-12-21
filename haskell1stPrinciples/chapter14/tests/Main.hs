{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Hspec
import Test.QuickCheck hiding (Positive)
import qualified Data.Map as M

import FirstPrinciples.Chapter14.Addition
import FirstPrinciples.Chapter14.Morse

import Refined

import FirstPrinciples.Chapter08.WordNumber
import Data.List (sort)

main :: IO ()
main = do
  hspec $ do
    describe "Addition" $ do
      it "1+1 is greater than 1" $ do
        (1+1) > 1 `shouldBe` True
      it "2+2 is equal to 4" $ do 
        (2+2) == 4 `shouldBe` True
      it "15 `dividedBy` 3 is (5, 0)" $ do
        15 `dividedBy` 3 `shouldBe` (5, 0)
      it "22 divided By 5 is 4 remainder 2" $ do
        22 `dividedBy` 5 `shouldBe` (4,2)
      it "1 `multiplyBy1 0 is 0" $ do
        1 `multiplyBy` 0 `shouldBe` 0
      it "2 `multiplyBy` 2 is 4" $ do
        2 `multiplyBy` 2 `shouldBe` 4
      it "x+1 is always greater than x" $ do
        property $ \x -> x + 1 > (x::Int)
    describe "WordModule Properties" $ do        
      it "WordNumber Module: The Digits List is always nonempty" $ do
        property prop_digits_list_nonempty
      it "WordNumber Module: The Digits Lists has always the same number of elements as the number of digits" $ do
        property prop_numdigits_equal_length
      it "WordNumber Module: The wordNumber function always returns an error on Negative Numbers" $ do
        property prop_negative_numbers_always_left
    describe "WordNumber Unit Tests: digitToWord" $ do
      it "returns zero for 0" $ do
        digitToWord 0 `shouldBe` "zero"
    describe "WordNUmber Unit Tests: digits" $ do
      it "returns [1] for 1" $ do
        let refinedOne = $$(refineTH 1) :: Refined (From 0) Int
        digits refinedOne `shouldBe` [1]
      it "returns [1,0,0] for 100" $ do
        let refinedOneHundred = $$(refineTH 100) :: Refined (From 0) Int
        digits refinedOneHundred `shouldBe` [1,0,0]
    let wordNumberConv :: Int -> Either () [Char]
        wordNumberConv n = case wordNumber n of
                             Left _ -> Left ()
                             Right s -> (Right s)
          
    describe "WprdNumber Unit Tests: wordNumber" $ do
      it "one-zero-zero given 100" $ do
        wordNumberConv 100 `shouldBe` Right "one-zero-zero"
      it "nine-zero-zero-one given 9001" $ do
        wordNumberConv 9001 `shouldBe` Right "nine-zero-zero-one"
    describe "For half" $ do
      it "halfIdentity should hold" $ do
        property prop_checkHalfIdentity
    describe "List Ordered" $ do
      it "ordered list property should hold" $ do
        property (prop_checkSortedList :: [Int] -> Bool)
    describe "Associativity/Commutativity" $ do
      it "Addition is associative" $ do
        property (plusAssociative :: Int -> Int -> Int -> Bool)
      it "Addition is commutative" $ do
        property (plusCommutative :: Int -> Int -> Bool)
      it "Multiplication is associative" $ do
        property ((\a b c -> a * (b * c) == (a * b) * c) :: Int -> Int -> Int -> Bool)
      it "Multiplication is commutative" $ do
        property ((\ a b -> a * b == b * a) :: Int -> Int -> Bool)
    describe "quot & rem obey the law" $ do
      it "quot and rem equals x" $ do
        property quot_rem_law
    describe "div & mod obey the law" $ do
      it "div and rem equals x" $ do
        property div_mod_law
    -- Exercise 6
    describe "Operator ^ Associativity & commutivity" $ do
      it "Contradict Associativity" $ do
        property prop_power_associative
      it "Contradict Commutivity" $ do
        property prop_power_commutative
    describe "2x Reverse a list" $ do
      it "Reverse a list:Int " $ do
        property (prop_reverse_2x::[Int] -> Bool)
      it "Reverse a list:Chars" $ do
        property (prop_reverse_2x::[Char]->Bool)
    describe "Properties of Compose and Application" $ do
      it "..." $ do
        property prop_compose_dollar
    describe "Check folding functions" $ do
      it "foldr (:) == (++)" $ do
        property prop_check_foldr_cons_plusplus
      it "foldr (++) [] == concat" $ do
        property prop_check_foldr_concat
  quickCheck prop_thereAndBackAgain




-- Excercise 3
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative a b = a + b == b + a

-- Excercise 1
-- half :: Refined  -> Int
half :: Fractional a => a -> a
half x = x/2

halfIdentity :: Double -> Double
halfIdentity = (*2).half

prop_checkHalfIdentity :: Double -> Bool
prop_checkHalfIdentity d = halfIdentity d == d 


-- Exercise 2
-- For any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(Nothing, True) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x>=y)
  
prop_checkSortedList :: (Ord a) => [a] -> Bool
prop_checkSortedList al = listOrdered $ sort al 


-- Generators for the digits function in FirstPrinciples.Chapter08.WordNumber
-- TODO fix this warning
instance Arbitrary (Refined (From 0) Int) where
  arbitrary = do
    pn <- ((arbitrary `suchThat` (\n -> n >= 0 ) :: Gen Int))
    let erpn = ((refine pn)::(Either RefineException (Refined (From 0) Int)))
    case erpn of
      Left th -> fail (show th)
      Right rpn -> return rpn

-- Exercise 5
newtype PosInt = PosInt { n::Int } deriving (Eq, Ord, Show)

quot_rem_law :: Int -> PosInt  -> Bool
quot_rem_law x (PosInt y ) = (quot x y)*y + (rem x y) == x

div_mod_law :: Int -> PosInt -> Bool
div_mod_law x (PosInt y) = (div x y)*y + (mod x y) == x 

instance Arbitrary PosInt where
  arbitrary = do
    pn <- ((arbitrary `suchThat` (\nx -> nx > 0)) :: Gen Int)
    return $ PosInt pn

-- Exercise 6
-- Use expectFailure
prop_power_associative :: Property
prop_power_associative = expectFailure
                         (forAll (arbitrary::Gen Int)
                          (((\a b c -> a^(b^c) == (a^b)^c))::(Int -> Int -> Int -> Bool)))

prop_power_commutative :: Property
prop_power_commutative = expectFailure
                         (forAll (arbitrary::Gen (PosInt))
                          (\(PosInt a) (PosInt b) -> a^b == b^a))

-- Excercise 7
prop_reverse_2x :: (Eq a) => [a] -> Bool
prop_reverse_2x al = (reverse.reverse) al  == id al 


-- Excercise 8
prop_compose_dollar :: Property
prop_compose_dollar = forAll (arbitrary::Gen Int) (\x -> ((id.id) $ x) == (id $ id $ x) )

-- Excercise 9
prop_check_foldr_cons_plusplus :: Property
prop_check_foldr_cons_plusplus = counterexample "Found a Counter Example"
                                 (forAll (arbitrary::Gen [Int])
                                   (\al bl -> (foldr (:) al  bl)  == bl ++ al))

prop_check_foldr_concat :: Property
prop_check_foldr_concat = forAll (arbitrary::Gen [[Char]])
                          (\al -> (foldr (++) [] al) == concat al )


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll charGen (\c -> ((charToMorse c) >>= morseToChar) == Just c)


trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]


genBool :: Gen Bool
genBool = choose (True, False)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genPair :: (Arbitrary a, Arbitrary b) => Gen (a,b)
genPair = do
  a <- arbitrary
  b <- arbitrary
  return (a,b)


genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a,b,c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a,b,c)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_additionGreaterFalsiable :: Int -> Bool
prop_additionGreaterFalsiable x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater


runQcF :: IO ()
runQcF = quickCheck prop_additionGreaterFalsiable

prop_digits_list_nonempty :: Refined (From 0) Int -> Bool
prop_digits_list_nonempty rpn = case digits rpn of
  [] -> False
  _ -> True

prop_numdigits_equal_length :: Refined (From 0) Int -> Bool
prop_numdigits_equal_length rpn = let
  digitListLength = length $ digits rpn
  n = unrefine rpn
  numDigits = length $ show n 
  in numDigits == digitListLength

prop_negative_numbers_always_left :: Int -> Bool
prop_negative_numbers_always_left n = case wordNumber n of
  Left _  | n <  0 -> True
  Right _ | n >= 0 -> True
  _                -> False
