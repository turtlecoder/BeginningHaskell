module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Chapter15.BinaryTrees

hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empt tree" $
  assertEqual "Insertion is wrong" (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

allTests::TestTree
allTests = testGroup "Tasty Tests" [
  testGroup "HUnit Tests" [ hunitTestInsertOnLeaf ]
  ]


main::IO ()
main = defaultMain allTests
