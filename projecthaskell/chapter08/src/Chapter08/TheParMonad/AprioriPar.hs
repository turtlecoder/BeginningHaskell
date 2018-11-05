{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Chapter08.TheParMonad.AprioriPar where

import Data.Default
import Control.Monad.Par
import qualified Data.Map as M
import Data.Foldable

-- Excercise 8-1 Parallel k-means
class (Default v, Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v


class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  -- since both are the same types
  toVector = id

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u, v) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0.0, 0.0) lst
                     n = fromIntegral $ length lst
                     in (u/n, v/n)


type VectorInitFunc  e v = Int -> [e] -> [v]


kMeans :: (NFData v, NFData e, Vector v, Vectorizable e v) => (VectorInitFunc e v) -> Int -> Double -> [e] -> [v]
kMeans initFunc n threshold points = runPar $ kMeans' initialCentroids points threshold
  where initialCentroids = initFunc n points

kMeans' :: (NFData v, NFData e, Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Par [v]
kMeans' centroids points threshold =
  do assignmentsV <- new
     put assignmentsV $ clusterAssignmentsPhase centroids points
     assignments <- get assignmentsV
     newCentroidsMap <- parNewCentroidPhase assignments
     if shouldStop (newCentroidsMap) threshold
       then return (fmap snd newCentroidsMap)
       else kMeans' (fmap snd newCentroidsMap) points threshold
     

clusterAssignmentsPhase :: (Vector v , Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentsPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      foldingFunc pt mapping =
        let choseCentroid =
              minimumBy (\x y -> compare (distance x $ toVector pt) (distance y $ toVector pt)) centroids
        in M.adjust (pt:) choseCentroid mapping
  in foldr foldingFunc initialMap points
            
      
     
parNewCentroidPhase :: (NFData v, Vector v, Vectorizable e v) => M.Map v [e] -> Par [(v,v)]
parNewCentroidPhase mapping =
  let mapSize = M.size mapping
      in if mapSize <= 5
         then return $ (M.toList.fmap (centroid.(fmap toVector))) mapping
         else let (leftMap, rightMap) = M.splitAt (mapSize `div` 2) mapping
              in do lVar <- spawn $ parNewCentroidPhase leftMap
                    rVar <- spawn $ parNewCentroidPhase rightMap
                    newL <- get lVar
                    newR <- get rVar
                    return $ newL ++ newR
           

shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold
