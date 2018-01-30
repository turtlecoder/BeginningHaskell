module Chapter15.BinaryTrees where

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf 
                  deriving Show

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert _ _ = undefined
