module Main where

import Test.HUnit

test1 = TestCase (assertEqual "for (foo, 3), " (1,2) (foo 3))

foo :: Int -> (Int, Int)
foo _ = (1,2)

tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()

