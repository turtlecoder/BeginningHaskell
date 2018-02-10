module Chapter15.BinaryTree

data BinaryTree : Type -> Nat -> Type where
  Leaf: BinaryTree a 0 
  Node: a -> BinaryTree a l -> BinaryTree a r -> BinaryTree a (S ( l + r ))
  
treeInsert : Ord a => a -> BinaryTree a n -> BinaryTree a (S n)
treeInsert x Leaf = Node x Leaf Leaf
treeInsert x (Node y l r) with (x<=y) | True = let l' = treeInsert x l in Node y l' r
                                      | False ?= let r' = treeInsert x r in Node y l r'


-- Chapter15.BinaryTree.treeInsert_lemma_1 = proof
--   intros
--   rewrite sym (plusSuccRightSucc l r)
--   trivial
-- qed

plusSuccRightSucc: (left:Nat) -> (right:Nat) -> S (left + right) = left + S (right)

treeMerge : Ord a => BinaryTree a n -> BinaryTree a m -> BinaryTree a ( n + m)
treeMerge t1 Leaf ?= t1
treeMerge t1 (Node x2 l2 r2) ?= treeInsert x2 (treeMerge (treeMerge t1 l2) r2)


