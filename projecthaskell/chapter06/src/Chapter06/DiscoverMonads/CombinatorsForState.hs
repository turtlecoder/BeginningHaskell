module Chapter06.DiscoverMonads.CombinatorsForState where

import qualified Data.Map as M
import Chapter06.KMeansCommon
import Data.List
import Chapter06.ThenDo.StateImpl

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int
                                 }


newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
-- newCentroids centroidMap = (M.elems . (fmap (centroid.map toVector))) centroidMap
newCentroids centroidMap = let newCentroidList = fmap (centroid.map toVector) centroidMap
                               newElems = M.elems newCentroidList
                               in newElems


clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
      in foldr (\aPoint accumMap -> let chosenCentroid =
                                          minimumBy (\x y -> compare (distance x $ toVector aPoint)
                                                                     (distance y $ toVector aPoint)) centrs
                               in M.adjust (aPoint:) chosenCentroid accumMap) initialMap points

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points =
  (\s -> (centroids s, s)) `thenDo`
  (\prevCenters -> (\s -> (clusterAssignments prevCenters points, s)) `thenDo`
  (\assignments -> (\s -> (newCentroids assignments, s)) `thenDo`
  (\newCentrs -> (\s -> ((), s { centroids = newCentrs })) `thenDo`
  (\_ -> (\s -> ((), s { steps = steps s + 1 })) `thenDo`
  (\_ -> (\s -> (threshold s, s)) `thenDo`
  (\t -> (\s -> (sum $ zipWith distance prevCenters newCentrs, s)) `thenDo`
  (\err -> if err < t then (\s -> (newCentrs, s)) else (kMeans' points))))))))
         
         
initialState :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState v
initialState i k pts t = KMeansState (i k pts) t 0

kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans i k pts t = fst $ kMeans' pts (initialState i k pts t)
