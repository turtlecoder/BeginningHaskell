{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter04.Containers where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree
import Data.Graph
import Chapter04.Typeclasses
import Chapter04.Datatypes
import Data.Monoid

m1 = M.singleton "hello" 3
m2 = M.insert "bye" 2 m1
m3 = M.insert "hello" 5 m2
m4 = M.insertWith (+) "hello" 7 m3
           

mapInsert :: Ord k => k -> a -> M.Map k a -> M.Map k a
mapInsert k a m = M.alter (\ _  -> Just a) k m

mapDelete :: Ord k => k -> M.Map k a -> M.Map k a
mapDelete             k    m = M.alter (\ _ -> Nothing) k m

mapAdjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
mapAdjust adjF k m = M.alter f' k m
  where f' = (\case (Just a) -> Just (adjF a)
                    Nothing  -> Nothing)



-- Excercise 4.3
-- Traverse the list and classify each element and create a map 
classifyClientsV1 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClientsV1 cl  = classifyClientsHelper cl M.empty
  where
    alterFunc hd (Just set) = (Just (S.insert hd set))
    alterFunc hd Nothing    = (Just (S.singleton hd))
    classifyClientsHelper []                         mc = mc
    classifyClientsHelper (hd @ (GovOrg _ _):tl)     mc = classifyClientsHelper tl (M.alter (alterFunc hd) GovOrgKind mc)
    classifyClientsHelper (hd @ (Company _ _):tl)    mc = classifyClientsHelper tl (M.alter (alterFunc hd) CompanyKind mc)
    classifyClientsHelper (hd @ (Individual _ _):tl) mc = classifyClientsHelper tl (M.alter (alterFunc hd) IndividualKind mc)
-- Make lists and then make a set of maps
classifyClientsV2 :: [Client Integer] -> M.Map ClientKind (S.Set (Client Integer))
classifyClientsV2 cl = makeSets $ classifyClientsHelper cl [] [] []
  where
    classifyClientsHelper [] govOrgList companyList individualList = (govOrgList, companyList, individualList)
    classifyClientsHelper ((hd @ (GovOrg _ _)):tl)     gl cl il = classifyClientsHelper tl (hd:gl) cl il
    classifyClientsHelper ((hd @ (Company _ _)):tl)    gl cl il = classifyClientsHelper tl gl (hd:cl) il
    classifyClientsHelper ((hd @ (Individual _ _)):tl) gl cl il = classifyClientsHelper tl gl cl (hd:cl)
    makeSets (gl,cl,il) = M.fromList [(GovOrgKind, S.fromList gl),
                                      (CompanyKind, S.fromList cl),
                                      (IndividualKind, S.fromList il)]
    
preOrder :: (a->b) -> Tree a -> [b]
preOrder f (Node v subtrees) = let subtreesTraversed = concat $ map (preOrder f) subtrees
                                   in (f v) : subtreesTraversed

pictureTree :: Tree Int
pictureTree = Node 1 [ Node 2 [ Node 3 []
                              , Node 4 []
                              , Node 5 [] ]
                     , Node 6 [] ]
-- Doing Some Graph Stuff

timeMachineGraph :: [(String, String, [String])]
timeMachineGraph = [ ("wood", "wood", ["walls"])
                   , ("plastic", "plastic", ["walls", "wheels"])
                   , ("aluminum", "aluminum", ["wheels", "door"])
                   , ("walls", "walls", ["done"])
                   , ("door", "door", ["done"])
                   , ("done", "done", [])]

timeMachinePrecedence :: (Graph, Vertex -> (String, String, [String]), String-> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

timeMachineTravel :: Graph
timeMachineTravel = buildG (103, 2013) [(1302, 1614), (1614,1302), (1302,2013),
                                        (2013, 1302), (1614,2013),
                                        (2013, 1408), (1408,1993), (1408,917),
                                        (1993, 917),  (907, 103),  (103,917)]




-- Excercise 4-4

instance Priceable (BookR) where
  getPrice BookR { priceB=p} = p

instance Priceable (TravelGuide) where
  getPrice TravelGuide { priceTG = p } = p

totalPrice::Priceable p => [p] -> Double
totalPrice products = foldl (\sum p -> sum + (getPrice p)) 0.0 products

treeFind1::TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = case t `compare` v of
                              EQ -> Just v
                              LT -> treeFind1 t l
                              GT -> treeFind1 t r
treeFind1 t Leaf1         = Nothing


treeInsert1 :: TravelGuide->BinaryTree1 -> BinaryTree1
treeInsert1 t (n@(Node1 v l r)) = case t `compare` v of
                                    EQ -> n
                                    LT -> Node1 v (treeInsert1 t l) r
                                    GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 t (Node2 v l r) = case t `compare` v of
                              EQ -> Just v
                              LT -> treeFind2 t l
                              GT -> treeFind2 t r



concatBinaryTree2 :: Ord a => BinaryTree2 a -> BinaryTree2 a -> BinaryTree2 a
concatBinaryTree2 n1 (Node2 b l2 r2) =let
  n1' = concatBinaryTree2 n1 l2
  n1'' = concatBinaryTree2 n1' r2
  in treeInsert2 b n1''

treeInsert3 :: (Ord c, Ord v) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v' c' n@(Node3 v c l r) = case v' `compare` v of
                                        EQ -> n
                                        LT -> Node3 v (min c c') (treeInsert3 v' c' l) r
                                        GT -> Node3 v (min c c') l (treeInsert3 v' c' r)
treeInsert3 v c Leaf3               = Node3 v c Leaf3 Leaf3

treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v' c' n@(Node3 v c l r) = case v `compare` v' of
                                      EQ -> n
                                      LT -> let newLeft = treeInsert4 v' c' l
                                                newCache = c <> cached newLeft <> cached r
                                            in Node3 v newCache newLeft r
                                      GT -> let newRight = treeInsert4 v' c' r
                                                newCache = c <> cached l <> cached newRight
                                            in Node3 v newCache l newRight
                                      
cached::Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty
