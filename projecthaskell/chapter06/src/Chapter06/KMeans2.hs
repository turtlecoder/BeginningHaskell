{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Chapter06.KMeans2 where

import Control.Lens
import Chapter06.KMeansCommon
import qualified Data.Map as M
import Data.List

data KMeansState e v = KMeansState { _centroids :: [v]
                                   , _points :: [e]
                                   , _err :: Double
                                   , _threshold :: Double
                                   , _steps :: Int
                                   }

makeLenses ''KMeansState

initializeState :: (VectorInitFunc e v) -> Int -> [e] -> Double -> KMeansState e v
initializeState initFunc n pts t = KMeansState ( initFunc n pts) pts (1.0/0.0) t 0


kMeans :: (Vector v, Vectorizable e v ) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans  i n pts t = view centroids $ kMeans' (initializeState i n pts t )

kMeans' :: (Vector v, Vectorizable e v) => KMeansState e v -> KMeansState e v
kMeans' state =
  let assignments = clusterAssignments (state^.centroids) (state^.points)
      state1 = state &
               centroids.traversed %~
               (\eachCentroid ->
                   centroid $ fmap toVector $ M.findWithDefault [] eachCentroid assignments)
      state2 = state1 & err .~sum (zipWith distance (state^.centroids) (state1^.centroids))
      state3 = state2 & steps +~ 1
   in if state3^.err < state3^.threshold then state3 else kMeans' state3

{- Excercise 6-3 -}

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      in foldr (\point mapAccum ->
                  let
                    chosenCentroid =
                      minimumBy (\x y -> compare (distance x $ toVector point) (distance y $ toVector point)) centroids
                  in
                    M.adjust (point:) chosenCentroid mapAccum)
               initialMap points
