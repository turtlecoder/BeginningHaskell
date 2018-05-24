{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Chapter06.DifferentStates.ReaderWriterRWS where

import Chapter06.KMeansCommon
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Control.Monad.RWS as RWS
import qualified Control.Monad.Writer as W
import qualified Data.Map as M
import Data.List (minimumBy)

data Person = Person { _firstName :: String, _lastName :: String }  deriving Show

data Settings e v = Settings { i :: Int -> [e] -> [v]                         
                             , k :: Int
                             , th :: Double
                             , user :: Person
                             }

kMeansMain :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) [v]
kMeansMain points = do i' <- asks i
                       k' <- asks k
                       t' <- asks th
                       return $ kMeans i' k' points t'

kMeans :: (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeans initFn steps threshold person = undefined


compareClusters :: (Vector v, Vectorizable e v) => [e] -> Reader (Settings e v) ([v], [v])
compareClusters points = do c1 <- kMeansMain points
                            -- Modify the local state
                            c2 <- local (\s -> s { k = k s + 1} ) (kMeansMain points)
                            return (c1,c2)

accessDatabase :: W.Writer String ()
accessDatabase = do W.tell "Start database access"
                    info <- readInformation
                    computeValue info
                    W.tell "Finish database access"

readInformation :: W.Writer String info
readInformation = undefined

computeValue :: info -> W.Writer String ()
computeValue info = undefined

-- Excercise 6-6
-- New Implementation of writer (redefining a tuple as a writer monad)
newtype MyWriter m a = MyWriter (a,[m])

instance (Monoid m, Applicative (MyWriter m)) => Monad (MyWriter m) where
  return a = MyWriter (a,mempty)
  (MyWriter (a,m)) >>= f = let MyWriter (b, m') = f a
                           in MyWriter (b, m `mappend` m')
  

tell m = MyWriter ((),m)

-- Using the RWS monad
kMeans' :: (Vector vector, Vectorizable points vector) => [points] -> RWS Double (Sum Int) [vector] ()
kMeans' points = do prevCenters <- get
                    let assignments = clusterAssignments prevCenters points
                        newCenters = newCentroids assignments
                    put newCenters
                    RWS.tell (Sum 1)
                    t <- ask
                    let err = sum $ zipWith distance prevCenters newCenters
                    unless (err < t) $ kMeans' points
                    


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

kMeansMain' :: (Vector vector, Vectorizable point vector) => (Int -> [point] -> [vector]) -> Int -> [point] -> Double -> ([vector], Sum Int)
kMeansMain' initFn n pts t = RWS.execRWS (kMeans' pts) t (initFn n pts)
