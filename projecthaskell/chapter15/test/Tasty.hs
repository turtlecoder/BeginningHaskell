module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Chapter15.BinaryTrees
import Data.Maybe


hunitTestInsertOnLeaf :: TestTree
hunitTestInsertOnLeaf = HU.testCase "Insert 'a' on empty tree" $
  assertEqual "Insertion is wrong" (treeInsert 'a' Leaf) (Node 'a' Leaf Leaf)

hunitTestInsertOnLeaf' = HU.testCase "Insert 'a' on empty tree" $
  treeInsert 'a' Leaf HU.@?= Node 'a' Leaf Leaf

hunitTestInsertOnLeaf'' = HU.testCase "Insert 'a' on empty tree" $
  Node 'a' Leaf Leaf HU.@?= treeInsert 'a' Leaf

-- Using an equality property

hunitTestInsertFind :: Ord a => a -> BinaryTree a -> TestTree
hunitTestInsertFind e t = HU.testCase "Insert can be found" $
  assertBool "Cannot find element" (isJust $ treeFind e $ treeInsert e t)

-- Excercise 15-1: Unit Testing Binary Trees
hunitTestIncNodeCount :: Ord a => a -> BinaryTree a -> TestTree
hunitTestIncNodeCount e t = HU.testCase "Increment Node Count when a node is inserted" $
  assertBool "Node Count not expected" ((1 + treeSize t) == (treeSize $ treeInsert e t))

hunitTestDecNodeCount :: (Ord a) => a -> BinaryTree a -> TestTree
hunitTestDecNodeCount e t = HU.testCase "Decrement Node Count when a node is removed" $
  assertBool "Node Count not expected" ( (treeSize t) - 1  == (treeSize $ treeRemove e t))

hunitTestNodeCountisZeroOnLeaf :: (Ord a) => a -> BinaryTree a -> TestTree
hunitTestNodeCountisZeroOnLeaf e t = HU.testCase "Node Count is 0 when a node is removed from an empty tree" $
  assertBool "Node Count is not 0" (0 == (treeSize $ treeRemove e t))

allTests::TestTree
allTests = testGroup "Tasty Tests" [
  testGroup "HUnit Tests" [ hunitTestInsertOnLeaf
                          , hunitTestInsertOnLeaf'
                          , hunitTestInsertOnLeaf''
                          , hunitTestInsertFind 'b' Leaf
                          , hunitTestInsertFind 'c' (Node 'd' Leaf Leaf)
                          , hunitTestIncNodeCount 'a' Leaf
                          , hunitTestIncNodeCount 'b' (Node 'a' Leaf Leaf)
                          , hunitTestDecNodeCount 'a' (Node 'a' Leaf Leaf)
                          , hunitTestNodeCountisZeroOnLeaf 'a' Leaf
                          ]
  ]


main::IO ()
main = defaultMain allTests
