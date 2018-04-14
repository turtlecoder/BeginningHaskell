{-# LANGUAGE InstanceSigs #-}
{- LANGUAGE NoImplicitPrelude -}

module Chapter04.BinaryTrees where

-- import GHC.Base hiding (Functor(Maybe))


data TravelGuide = TravelGuide { _title :: String
                               , _authors :: [String]
                               , _price :: Double
                               } deriving (Show, Eq, Ord)

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving Eq

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1==p2 && (t1 < t2) || (t1 == t2 && a1 <=a2))


data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving Show

treeFind :: Ord a => BinaryTree a -> a -> Maybe a
treeFind Leaf a = Nothing
treeFind (Node a lt rt) a' = case compare a' a of
                               EQ -> Just a'
                               LT -> treeFind lt a'
                               GT -> treeFind rt a'
  

-- Excercise 4-7: More operations on Binary Trees

treeInsert :: Ord a => BinaryTree a -> a -> BinaryTree a
treeInsert t@(Node a lt rt) a' = case compare a' a of
                                   EQ -> t
                                   LT -> Node a (treeInsert lt a') rt
                                   GT -> Node a lt (treeInsert rt a')
                                   

-- Concatenate two trees
treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge t Leaf = t
treeMerge t (Node a lt rt) = treeInsert (treeMerge (treeMerge t lt) rt) a


-- Binary Trees with Monoidal Cache
data BinaryTreeMC a c = NodeMC a c (BinaryTreeMC a c) (BinaryTreeMC a c)
                      | LeafMC

-- Minimum Binary Search Tree with minimum cached
treeInsertMinCache :: (Ord a, Ord c) => BinaryTreeMC a c -> c -> a -> BinaryTreeMC a c
treeInsertMinCache LeafMC c a' = NodeMC a' c LeafMC LeafMC 
treeInsertMinCache (NodeMC a c lt rt) c' a' = case compare a' a of
                                          LT -> NodeMC a (min c c') (treeInsertMinCache lt c' a') rt
                                          GT -> NodeMC a (min c c') lt (treeInsertMinCache lt c' a')
                                          EQ -> NodeMC a c lt rt
                                          

-- Minimum Tree with Monoidal Cached
treeInsertMC :: (Ord a, Monoid c) => BinaryTreeMC a c -> a -> c -> BinaryTreeMC a c
treeInsertMC LeafMC a' c' = NodeMC a' c' LeafMC LeafMC
treeInsertMC (NodeMC a c lt rt) a' c' = case compare a' a of
                                          EQ -> NodeMC a c lt rt
                                          LT ->
                                            let newLeft = treeInsertMC lt a' c'
                                                newCache = c `mappend` cached newLeft `mappend` cached rt
                                            in
                                            NodeMC a newCache newLeft rt
                                          GT ->
                                            let newRight = treeInsertMC rt a' c'
                                                newCache = c `mappend` cached lt `mappend` cached newRight
                                                in
                                              NodeMC a newCache lt newRight
cached :: (Monoid c) => BinaryTreeMC a c -> c
cached (NodeMC _ c _ _) = c
cached LeafMC           = mempty

newtype Min = Min Double deriving Show

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend (Min x) (Min y) = Min $ (min x y)


-- Excercise 4-8: Functor Fun!
-- Can be done in many ways, but newtype should have one constructor and only one argument

newtype BTMaybe a = BTMaybe (Maybe a)

instance Functor BTMaybe where
  fmap fn (BTMaybe (Just a)) = BTMaybe (Just (fn a))
  fmap fn (BTMaybe Nothing)  =  BTMaybe Nothing


-- Functor Instance for BinaryTree

-- Maintaining a balanced tree in Functor map is not possible, because Functor
-- laws should be mapped over all kinds of a, b

instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node a lt rt) = Node (f a) (fmap f lt) (fmap f rt)

-- instance Functor (BinaryTree) where
--   fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
--   fmap fn tr = let -- inOrderMap :: (Ord b) => (BinaryTree b) -> (BinaryTree a) -> (a->b) -> BinaryTree b
--                    inOrderMap _ Leaf _ = Leaf
--                    inOrderMap accumTree (Node a lt rt) fn =
--                      treeInsert (inOrderMap (inOrderMap accumTree lt fn) rt fn) (fn a)
--                in
--                  inOrderMap Leaf tr fn
-- To Balance a tree, you need a function

balanceTree :: (Ord a) => BinaryTree a -> BinaryTree a
balanceTree (Node a lt rt) = treeInsert (treeMerge lt rt) a

-- Returns a funtion to transform the tree
binTreeMap :: (Ord b)=>(a->b) -> (BinaryTree a -> BinaryTree b)
binTreeMap f = balanceTree.(fmap f)

