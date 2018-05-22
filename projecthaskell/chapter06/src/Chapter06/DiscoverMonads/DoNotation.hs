{-# LANGUAGE TemplateHaskell #-}

module Chapter06.DiscoverMonads.DoNotation where

import Control.Lens
import Chapter06.KMeansCommon
import qualified Data.Map as M
import Control.Monad.State
import Data.List

purchaseValueWithDo::(Num price) => purchaseId -> Maybe price
purchaseValueWithDo  purchaseId = do n <- numberItemsByPurchaseId purchaseId
                                     productId <- productByPurchaseId purchaseId
                                     price <- priceByProductId productId
                                     return $ fromInteger n * price

numberItemsByPurchaseId::purchaseId -> Maybe numberItem
numberItemsByPurchaseId purchaseId = undefined

productByPurchaseId :: (Num productId) => purchaseId -> Maybe productId
productByPurchaseId purchaseId = undefined

priceByProductId :: (Num price, Num productId) => productId -> Maybe price
priceByProductId productId = undefined


data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int
                                 }

-- makeLenses ''KMeansState

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


kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points = do prevCentrs <- fmap centroids get
                    let assignments = clusterAssignments prevCentrs points
                        newCentrs = newCentroids assignments
                    modify (\s -> s { centroids = newCentrs })
                    modify (\s -> s { steps = steps s + 1})
                    t <- fmap threshold get
                    let err = sum $ zipWith distance prevCentrs newCentrs
                    if err < t then return newCentrs else kMeans' points
                    
