module KMeans4 where

import VectorData
import Data.List
import qualified Data.Map as M
import Control.Monad.State
import KMeansData

clusterAssignments :: (Vector v , Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centrs
                            in M.adjust (p:) chosenCentroid m )
         initialMap points

newCentroids :: (Vector v, Vectorizable e v ) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid. map toVector)


kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points = do prevCentrs <- fmap centroids get
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    modify (\s ->  s { centroids = newCentrs })
                    modify (\s -> s { steps = steps s + 1 })
                    t <- fmap threshold get
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    if err < t then return newCentrs else kMeans' points


kMeans :: (Vector v, Vectorizable e v) => Int -> [e] -> Double -> [v]
kMeans n pts t = evalState (kMeans' pts) (initializeState n t)


initializeState :: Int -> Double -> KMeansState v
initializeState n t = undefined
                 
