{-# LANGUAGE RankNTypes #-}

module Chapter06.DifferentStates.MutableRefsWithST where

import Control.Monad.ST
import Data.STRef
import Chapter06.KMeansCommon
import qualified  Data.Map as M
import Data.List (minimumBy)

listLength :: [a] -> Integer
listLength alist = runST $ do l <- newSTRef 0
                              traverseList alist l
                              readSTRef l
                                where traverseList []     _ = return ()
                                      traverseList (_:xs) l = do modifySTRef' l (+1)
                                                                 traverseList xs l

kMeans' :: (Num steps, Vector point, Vectorizable point centroid) => [point] -> Double -> ([centroid], steps)
kMeans' points threshold = runST $ do
  stepsRef <- newSTRef 0
  centroidsRef <- newSTRef ([]::[centroid])
  kMeansST threshold stepsRef centroidsRef
    where
      kMeansST threshold stepsRef centroidsRef = do 
        step <- readSTRef stepsRef
        centroids <- readSTRef centroidsRef
        writeSTRef stepsRef (step + 1)
        let assignments = clusterAssignments centroids points
            newCenters = newCentroids assignments
        writeSTRef centroidsRef newCenters
        let err = sum $ zipWith distance centroids newCenters
        if err < threshold then return (newCenters, step)
          else kMeansST threshold stepsRef centroidsRef
 
clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\aPoint accumMap -> let chosenCentroid =
                                          minimumBy (\x y -> compare (distance x $ toVector aPoint)
                                                                     (distance y $ toVector aPoint)) centrs
                               in M.adjust (aPoint:) chosenCentroid accumMap) initialMap points

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
-- newCentroids centroidMap = (M.elems . (fmap (centroid.map toVector))) centroidMap
newCentroids centroidMap = let newCentroidList = fmap (centroid.map toVector) centroidMap
                               newElems = M.elems newCentroidList
                               in newElems
