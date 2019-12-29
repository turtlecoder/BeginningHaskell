module Chapter04.Trees where

import Data.Tree
import Data.Foldable

preOrder :: (a->b) -> Tree a -> [b]
preOrder f (Node v subTrees)  = let subTreesTraversed = concat $ map (preOrder f) subTrees
                                in (f v) : subTreesTraversed

pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 []]


flattenPictureTree = flatten pictureTree

levelsPictureTree = levels pictureTree

fmapPictrueTree = fmap (*2) pictureTree

foldablePictureTree = Data.Foldable.foldr (+) 0 pictureTree

-- >>> pictureTree
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 3, subForest = []},Node {rootLabel = 4, subForest = []},Node {rootLabel = 5, subForest = []}]},Node {rootLabel = 6, subForest = []}]}

-- >>> flattenPictureTree
-- [1,2,3,4,5,6]

-- >>> levelsPictureTree
-- [[1],[2,6],[3,4,5]]
