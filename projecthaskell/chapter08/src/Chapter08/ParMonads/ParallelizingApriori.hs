{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Chapter08.ParMonads.ParallelizingApriori where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad.Par
import Control.DeepSeq
import Data.Default
import Data.List

-- Clients
data Client = GovOrg { _clientName :: String }
            | Company { _clientName :: String
                      , _person :: Person
                      , _duty :: String
                      }
            | Individual { _person :: Person }
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     , gender :: Gender
                     }
              deriving (Show, Eq, Ord)

data Gender = Male | Female | UnknownGender deriving (Show, Eq, Ord)

-- Products
data Product = Product { productId :: Integer
                       , productType :: ProductType
                       } deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TraveGuide | Tool | Trip deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client
                         , products :: [Product]
                         } deriving (Show, Eq, Ord)
                   


data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchaseProduct Integer
                  | InfoPurchaseProductType ProductType
                  deriving (Show, Eq, Ord)



newtype Transaction = Transaction (S.Set PurchaseInfo) deriving (Eq, Ord)


newtype FrequentSet = FrequentSet (S.Set PurchaseInfo) deriving (Eq, Ord)

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  let c1 = noDups $ concatMap (\(Transaction t) -> map (FrequentSet . S.singleton) $ S.toList t) transactions
      l1NotFiltered = map (\fs -> (fs, setSupport transactions fs > minSupport)) c1
  in concatMap (\(fs, b) -> if b then [fs] else []) l1NotFiltered

-- | Support of the rule is defined at the ratio of transactions that contain the item with
-- the total transactions
setSupport :: [Transaction] -> FrequentSet -> Double
setSupport transactions (FrequentSet sElements) =
  let total = length transactions
      supp = length (filter (\(Transaction tElements) -> sElements `S.isSubsetOf` tElements ) transactions)
      in fromIntegral supp / fromIntegral total


generateNextLk :: Double -> [Transaction] -> (Int, [FrequentSet]) -> Maybe ([FrequentSet], (Int, [FrequentSet]))
generateNextLk _ _ (_, []) = Nothing
generateNextLk minSupport transactions (k, lk) =
  let ck1 = noDups $ [ FrequentSet $ a `S.union` b | FrequentSet a <- lk, FrequentSet b <- lk
                                                   , S.size (a `S.intersection` b) == k - 1]
      lk1 = runPar $ filterLk minSupport transactions ck1
  in Just (lk1, (k+1, lk1))

instance NFData FrequentSet where
  rnf (FrequentSet !_) = ()

filterLk :: Double -> [Transaction] -> [FrequentSet] -> Par [FrequentSet]
filterLk minSupport transactions frequentSetList =
  let lengthFrequentSetList = length frequentSetList
  in
    if lengthFrequentSetList <= 5
    then return $ filter (\fs -> setSupport transactions fs > minSupport) frequentSetList
    else let (l,r) = splitAt (lengthFrequentSetList `div` 2) frequentSetList
         in do lVar <- spawn $ filterLk minSupport transactions l
               lFiltered <- get lVar
               rVar <- spawn $ filterLk minSupport transactions r
               rFiltered <- get rVar
               return $ lFiltered ++ rFiltered

-- Excercise 8-1: Parallel K-Means

class (Default v, Ord v) => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v



class Vector v => Vectorizable e v where
  toVector :: e -> v 

instance Vectorizable (Double, Double) (Double, Double) where
  -- since both are the same types
  toVector = id

instance Vector (Double, Double) where
  distance (a,b) (c,d) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  centroid lst = let (u,v) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0.0,0.0) lst
                     n = fromIntegral $ length lst
                 in (u/n, v/n)

type VectorInitFunc e v = Int -> [e] -> [v]



parNewCentroidPhase :: (NFData v, Vector v, Vectorizable e v) => M.Map v [e] -> Par [(v,v)]
parNewCentroidPhase mapping = let mapSize = M.size mapping
                              in if mapSize <= 5
                                then return $ (M.toList.fmap (centroid.(fmap toVector))) mapping
                                else
                                  -- split the list
                                  let (leftMap, rightMap) = M.splitAt (mapSize `div` 2) mapping
                                  in do lVar <- spawn $ parNewCentroidPhase leftMap
                                        rVar <- spawn $ parNewCentroidPhase rightMap
                                        newL <- get lVar
                                        newR <- get rVar
                                        return $ newL ++ newR
                                        

clusterAssignmentsPhase :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentsPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
      foldingFunc pt mapping =
        let chosenCentroid =
              minimumBy (\x y -> compare (distance x $ toVector pt) (distance y $ toVector pt))
                        centroids
        in M.adjust (pt:) chosenCentroid mapping
  in foldr foldingFunc initialMap points
                                        
kMeans' :: (NFData v, NFData e, Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Par [v]
kMeans' centroids points threshold = 
  do assignmentsV <- new
     put assignmentsV $ clusterAssignmentsPhase centroids points
     assignments <- get assignmentsV
     newCentroidsMap <- parNewCentroidPhase assignments
     if shouldStop (newCentroidsMap) threshold
       then return (fmap snd newCentroidsMap)
       else kMeans' (fmap snd newCentroidsMap) points threshold
     
shouldStop :: (Vector v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold
