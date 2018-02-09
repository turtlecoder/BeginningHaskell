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

treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind _ Leaf = Nothing
treeFind e (Node v lt rt) = case compare e v of
                              EQ -> Just v
                              LT -> treeFind e lt
                              GT -> treeFind e rt


treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node x l r ) = treeInsert x $ treeMerge (treeMerge t l ) r

treeSize :: BinaryTree a -> Integer
treeSize t = let treeSizeRT :: Num n => BinaryTree a -> n -> n
                 treeSizeRT Leaf n = n
                 treeSizeRT (Node _ lt rt) n = let ltSize = treeSizeRT lt n
                                                   rtSize = treeSizeRT rt n
                                               in 1 + ltSize + rtSize
             in
               treeSizeRT t 0

treeRemove :: (Eq a, Ord a) => a -> BinaryTree a -> BinaryTree a
treeRemove _ Leaf = Leaf
treeRemove e (Node a lt rt) = if e==a
                              then treeMerge lt rt
                              else treeInsert a $ treeMerge ltn rtn
  where ltn = treeRemove e lt
        rtn = treeRemove e rt
