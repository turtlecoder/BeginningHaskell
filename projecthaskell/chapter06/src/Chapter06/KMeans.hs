
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter06.KMeans where

import Data.List
import qualified Data.Map as M
import Data.Default

import Chapter06.KMeansCommon



-- Excercise 6-1: Counting the Number of Steps
-- K-Means Implementation
-- Args
-- Vectors Initialization function
-- Number of Centroids
-- Data (The information)
-- Final Centroids with Number of Iterations
kMeans::(Vector v, Vectorizable e v) => (VectorInitFunc e v) -> Int -> Double -> [e] -> ([v],Int)
kMeans initFunc n threshold points = kMeans' initialCentroids points threshold 0
  where
    initialCentroids = initFunc n points

    

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Int -> ([v], Int)
kMeans' centroids points threshold n =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in
    if shouldStop oldNewCentroids threshold
    then (newCentroids, n)
    else kMeans' newCentroids points threshold (n+1)

-- Assigns new points to a centroids
clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      foldingFunc pt map =
        let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector pt)
                                                        (distance y $ toVector pt)) 
                                       centroids
        in M.adjust (pt:) chosenCentroid map
  in foldr foldingFunc  initialMap points

-- >>> zip [(1,2), (2,3), (5,6)] (repeat [])
-- [((1,2),[]),((2,3),[]),((5,6),[])]

-- >>> :t M.fromList
-- M.fromList :: Ord k => [(k, a)] -> M.Map k a

-- >>> :t M.adjust
-- M.adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a

-- Calculates a new centroid for each old centroid and returns a map to them
-- this point-style composition is not a very beginner friendly
-- Here's whats happening here
-- 1. Convert each cluster (list of points) to a cluster of Vectors-concepts
-- 2. Apply the centroid function each cluster -> Get the list of new centroids
-- 3. Convert into a list of (old clusters -> new clusters) (scala style)
newCentroidPhase :: (Vector v, Vectorizable e v ) => M.Map v [e] -> [(v,v)]
newCentroidPhase mapping = (M.toList . fmap (centroid . (fmap toVector))) mapping

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n e = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) e


kMeansTest = kMeans initializeSimple 2 0.00001 ([(1,1), (1,2), (4,4), (5,5),(7,7),(-1,0),(10,7)]::[(Double, Double)])

-- >>> kMeansTest
-- ([(0.3333333333333333,1.0),(6.5,5.75)],2)


