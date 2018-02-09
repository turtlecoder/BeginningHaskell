{-# LANGUAGE ScopedTypeVariables #-}



module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Test.Tasty.QuickCheck as QC
import Chapter15.BinaryTrees
import Chapter15.BuggyLists
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

instance Arbitrary a => Arbitrary (BinaryTree a) where
  arbitrary = sized $ \n ->
    if (n==0)
    then return Leaf
    else frequency [(n, return Leaf), (n, resize (n-1) arbitrary)]

shrink Leaf = []
shrink (Node _ l r) = [l,r]

qcTestInsert :: TestTree
qcTestInsert = QC.testProperty "insert => you will find it" $
  \(n::Integer) t -> treeFind n (treeInsert n t) == Just n
  

qcTestDelete :: TestTree
qcTestDelete = QC.testProperty "delete => you wont find it" $
  \(n::Integer) t -> (treeFind n $ treeRemove n $ treeInsert n t) == Nothing

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

-- Excercise 15-2: Quickchecking Reverse  

reverseTests :: TestTree
reverseTests = testGroup "Tests Over reverse"
  [ QC.testProperty "reverse respects length" $
    \ (lst::[Integer]) -> length (reverse' lst) == length lst,
    QC.testProperty "reverse'.reverse is an identity" $
    \(lst::[Integer])-> (reverse'.reverse) lst == lst,
    -- This test seems like cheating, :-)
    QC.testProperty "head of a reversed list == last of the list" $
    \(lst::[Integer]) -> if not (null lst)
                         then (head.reverse') lst == (last lst)
                         else True
  ]


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
                          , reverseTests
                          , qcTestInsert
                          , qcTestDelete
                          ]
  ]


main::IO ()
main = defaultMain allTests
