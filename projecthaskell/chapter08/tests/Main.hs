module Main where

import Test.Hspec
import Chapter08.TheParMonad.Futures
import Chapter08.STM.ConcurrentResources 

-- import Test.HUnit

-- test1 = TestCase (assertEqual "for (foo, 3), " (1,2) (foo 3))

-- foo :: Int -> (Int, Int)
-- foo _ = (1,2)

-- tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1+1) > 1 `shouldBe` True
    it "2+2 is equal to 4" $ do
      (2+2) `shouldBe` 4
    it "15 dividedby 3 is 5" $ do
      15 `dividedBy` 3 `shouldBe` (5, 0)
    it "22 dividedBy 5 is 4 remainder 2" $ do
      22 `dividedBy` 5 `shouldBe` (4, 2)
  describe "Testing Chapter08.TheParMonad.Futures" $ do
    it "findTwofactors 123300 24256" $ do
      (findTwoFactors 123300 24256) `shouldNotBe` ((mempty, mempty)::([Integer], [Integer]))
    it "findTwoFactors 123300 245256 should match" $ do
      (findTwoFactors 123300 24256) `shouldSatisfy` (\(_:_, _:_) -> True)
  describe "Testing Chapter08.STM.ConcurrentResources" $ do
    -- testing simple IO actions
    res1 <- (runIO mainUpdateMoney)
    it "mainUpdateMoney" $ do
      res1 `shouldBe` ()
    res2 <- runIO mainRandomUpdatesReads
    it "mainRandomUpdatesReads" $ do
      res2 `shouldBe` ()
    -- How to redirect stdout to buffer in haskell??
  

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n-d) d (count+1)

-- This is not what we want,

summation :: Integral a => a -> a -> a
summation a b = let
  goSum total counter = if (counter == b)
                        then total
                        else goSum (total+1) (counter+1)
  in goSum a 0
     
