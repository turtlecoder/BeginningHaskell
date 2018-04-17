module KMeans where

import LensModule2
import VectorData
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.RWS
import Data.List
import KMeansData

data Settings e v = Settings { i :: Int ->[e] -> [v]
                             , k :: Int
                             , th :: Double
                             , user :: Person
                             }


kMeans :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> ([v], Sum Int)
-- kMeans i k pts t = fst $ kMeans' pts (initialState  i k pts t)
kMeans i n pts t = execRWS (kMeans' pts) t (i n pts)


kMeans' :: (Vector v, Vectorizable e v) => [e] -> RWS Double (Sum Int) [v] ()
kMeans' points = do prevCentrs <- get
                    let assignments = clusterAssignmentPhase prevCentrs points
                        newCentrs = newCentroid assignments
                    put newCentrs
                    tell (Sum 1)
                    t <- ask
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    unless (err < t) $ kMeans' points

clusterAssignmentPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = 
  let initialMap = M.fromList $ zip centroids (repeat [])
      in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centroids
                            in M.adjust (p:) chosenCentroid m)
               initialMap points
  
         


newCentroid:: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroid = M.elems . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold
