module KMeans where

import VectorData
import Data.Default
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Data.List

-- Implement the K-means algorithm using the ST monad. In particular, you must create one STRef for holding the centroids that will be updated and another one for the number of iterations.

clusterAssignments :: (Vector v , Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centrs
                            in M.adjust (p:) chosenCentroid m )
         initialMap points

newCentroids :: (Vector v, Vectorizable e v ) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid. map toVector)

-- Excercise 6-7 Solutin, Untested, Probably works

kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> (Int, [v])
kMeans' centroids points threshold = runST $ do
  stepST <- newSTRef 0
  centroidsST <- newSTRef centroids
  kMeansST threshold stepST centroidsST
    where
      kMeansST th stepST centroidsST = do
        step' <- readSTRef stepST
        centroids' <- readSTRef centroidsST
        writeSTRef stepST (step'+1)
        let assignments = clusterAssignments centroids' points
            newCenters = newCentroids assignments
        writeSTRef centroidsST newCenters
        let err = sum $ zipWith distance centroids' newCenters
        if err < th then (return (step', newCenters))
        else kMeansST th stepST centroidsST

        
        
        
  -- newCentroids <- modifySTRef (read centroids' (newCentroids assignments)
  -- step1 <- modifySTRef step' 
  


