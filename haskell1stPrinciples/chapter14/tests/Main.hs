module Main where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M

import FirstPrinciples.Chapter14.Addition
import FirstPrinciples.Chapter14.Morse

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
  quickCheck prop_thereAndBackAgain


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



