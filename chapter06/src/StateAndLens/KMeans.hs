module KMeans where

import KMeansDataLens
import VectorData
import Control.Monad.State
import qualified Data.Map as M
import Data.List
import Control.Lens

kMeans' :: (Vector v, Vectorizable e v ) => [e] -> State (KMeansState v) [v]
kMeans' points = do prevCentrs <- use centroids
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    centroids .= newCentrs
                    steps += 1
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    t <- use threshold
                    if err < t then return newCentrs else kMeans' points


clusterAssignments :: (Vector v , Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centrs
                            in M.adjust (p:) chosenCentroid m )
         initialMap points


newCentroids :: (Vector v, Vectorizable e v ) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid. map toVector)

