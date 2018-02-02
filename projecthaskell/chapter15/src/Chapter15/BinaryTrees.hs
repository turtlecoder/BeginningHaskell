-- | Simple Implementation of Binary Trees
module Chapter15.BinaryTrees where

-- | * A typical Binary Tree, The main Binary Tree
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) -- ^Inner Nodes
                  | Leaf                                 -- ^Leaves
                  deriving (Show,Eq)

{-|
Inserts an element into a 'BinaryTree'.

* If it finds a leaf, insert there

* If smaller than the item on the node, insert into the left.

* If larger then the item on the node, insert to the right.
>>> treeInsert 1 Leaf
Node 1 Leaf Leaf
-}
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) | x <= y  = Node y (treeInsert x l) r
                          | otherwise = Node y l (treeInsert x r)




treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node x l r ) = treeInsert x $ treeMerge (treeMerge t l ) r 
