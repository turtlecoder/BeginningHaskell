{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Test.Hspec
import Chapter08.TheParMonad.Futures
import Chapter08.STM.ConcurrentResources
import Chapter08.STM.AtomicTransactions.MVar
import Chapter08.STM.AtomicTransactions.TVars
import Chapter08.STM.AtomicTransactions.TimeMachineStore
import Chapter08.STM.ProducerConsumerQueues

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
    res <- runIO mainUpdateMoney
    it "mainUpdateMoney" $ do
      res `shouldBe` ()
    res <- runIO mainRandomUpdatesReads
    it "mainRandomUpdatesReads" $ do
      res `shouldBe` ()
  describe "Testing Chapter08.STM.AtomicTransactions.MVar" $ do
    res <- runIO mainAtomicTransactionsMVar
    it "mainAtomicTransactionsMVar" $ do
      res `shouldBe` ()
  describe "Testing Chapter08.STM.AtomicTransactions.TVar" $ do
    res4 <- runIO mainAtomicTransactionsTVar
    it "mainAtomicTransactionsTVar" $ do
      res4 `shouldBe` ()
  describe "Ex 8-2: Testing Time Machine Store Simulation" $ do
    res5 <- runIO timeMachinesSimulationIO
    it "The function should return ()" $ do
      res5 `shouldBe` ()
  describe "Ex 8-3: Testing Time Machine Store Simulation" $ do
    res6 <- runIO mainProducerConsumer
    it "The function mainProducerConsumer should return ()" $ do 
      res6 `shouldBe` ()
  

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
     
