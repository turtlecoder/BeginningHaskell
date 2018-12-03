{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Test.Hspec
import Test.QuickCheck hiding (Positive)
import qualified Data.Map as M

import FirstPrinciples.Chapter14.Addition
import FirstPrinciples.Chapter14.Morse

import Refined

import FirstPrinciples.Chapter08.WordNumber

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
      it "WordNumber Module: The Digits List is always nonempty" $ do
        property prop_digits_list_nonempty
      it "WordNumber Module: The Digits Lists has always the same number of elements as the number of digits" $ do
        property prop_numdigits_equal_length
      it "WordNumber Module: The wordNumber function always returns an error on Negative Numbers" $ do
        property prop_negative_numbers_always_left
  quickCheck prop_thereAndBackAgain

-- Generators for the digits function in FirstPrinciples.Chapter08.WordNumber
-- TODO fix this warning
instance Arbitrary (Refined (From 0) Int) where
  arbitrary = do
    pn <- ((arbitrary `suchThat` (\n -> n>=0)) :: Gen Int)
    let erpn = ((refine pn)::(Either RefineException (Refined (From 0) Int)))
    case erpn of
      Left th -> fail (show th)
      Right rpn -> return rpn




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
