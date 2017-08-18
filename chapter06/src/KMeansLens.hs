{-# LANGUAGE TemplateHaskell #-}

module KMeans2 where

import Control.Lens
import VectorData
import qualified Data.Map as M
import Data.List

data KMeansState e v = KMeansState { _centroids :: [v]
                                   , _points :: [e]
                                   , _err :: Double
                                   , _threshold :: Double
                                   , _steps :: Int
                                   }

makeLenses ''KMeansState

initializeState :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansState e v
initializeState i n pts t = KMeansState ( i n pts) pts (1.0/0.0) t 0


kMeans :: (Vector v, Vectorizable e v ) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans  i n pts t = view centroids $ kMeans' (initializeState i n pts t )

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignments (state^.centroids) (state^.points)
      state1 = state  & centroids.traversed %~ (\c -> centroid $ fmap toVector $ M.findWithDefault [] c assignments)
      state2 = state1 & err .~sum (zipWith distance (state^.centroids) (state1^.centroids))
      state3 = state2 & steps +~ 1
   in if state3^.err < state3^.threshold then state3 else kMeans' state3

{- Excercise 6-4 -}                                                          
                                   
clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      in foldr (\p m -> let chosenCentroid = minimumBy (\x y -> compare (distance x $ toVector p) (distance y $ toVector p)) centroids
                            in M.adjust (p:) chosenCentroid m)
               initialMap points


