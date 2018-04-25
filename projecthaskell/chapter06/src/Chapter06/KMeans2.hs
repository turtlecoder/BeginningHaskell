{-# LANGUAGE TemplateHaskell #-}
module Chapter06.KMeans2 where

import Control.Lens
import Chapter06.KMeansCommon

data KMeansState e v = KMeansState { _centroids::[v]
                                   , _points::[e]
                                   , _err::Double
                                   , _threshold::Double
                                   , _steps:: Int
                                   }

makeLenses ''KMeansState

vectorInitFunc :: VectorInitFunc (Double, Double) (Double, Double)
vectorInitFunc _ _ = undefined

-- Initialize the State function
initializeState :: (VectorInitFunc e v) -> Int -> [e] -> Double -> KMeansState e v
initializeState initFn n pts threshold = KMeansState (initFn n pts) pts (1.0/0.0) threshold 0

kMeans::(Vector v, Vectorizable e v) => (VectorInitFunc e v) -> Int -> [e] -> Double -> [v]
kMeans _ _ _ = undefined
