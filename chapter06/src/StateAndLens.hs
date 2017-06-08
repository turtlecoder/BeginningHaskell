{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module StateAndLens where

import Control.Lens
import Control.Lens.Getter
import Common
import Control.Monad.State
import RSData
import Data.Char
import Control.Monad.RWS


data KMeansState v = KMeansState { _centroids :: [v]
                                 , _threshold :: Double
                                 , _steps     :: Int
                                 }
makeLenses ''KMeansState

clusterAssignments centrs pts = undefined

newCentroids assignments = undefined

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points  = do prevCentrs <- use centroids
                     let assignments = clusterAssignments prevCentrs points
                         newCentrs = newCentroids assignments
                     centroids .= newCentrs
                     steps += 1
                     let err = sum $ zipWith distance prevCentrs newCentrs
                     t <- use threshold
                     if err < t then return newCentrs else kMeans' points

kMeans'' :: (Vector v, Vectorizable e v ) => [e] -> RWS Double (Sum Int) [v] ()
kMeans'' points = do prevCentrs <- get
                     let assignments = clusterAssignments prevCentrs points
                         newCentrs = newCentroids assignments
                     put newCentrs
                     tell (Sum 1)
                     t <- ask
                     let err = sum $ zipWith distance prevCentrs newCentrs
                     unless (err < t) $ kMeans'' points

kMeansRWS :: (Vector v, Vectorizable e v ) => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> ([v], Sum Int)
kMeansRWS i n pts t = execRWS (kMeans'' pts) t ( i n pts)
                     

data ExampleState = ExampleState { _increment :: Int
                                 , _clients :: [Client Int]
                                 } deriving Show
makeLenses ''ExampleState


zoomExample :: State ExampleState ()
zoomExample = do n <- use increment
                 zoom (clients.traversed) $ do
                   identifier += n
                   person.fullName %= map toUpper

                     
