module KMeansReader where

import LensModule2
import VectorData

import Control.Monad.Reader


data Settings e v = Settings { i :: Int ->[e] -> [v]
                             , k :: Int
                             , th :: Double
                             , user :: Person
                             }

kMeansMain :: (Vector v, Vectorizable e v ) => [e] -> Reader (Settings e v) [v]
kMeansMain points = do i' <- asks i
                       k' <- asks k
                       t' <- asks t
                       return $ kMeans i' k' points t'

  
