{-# LANGUAGE TemplateHaskell #-}
module Chapter06.KMeans2 where

import Control.Lens

data KMeansState e v = KMeansState { _centroids::[v]
                                   , _points::[e]
                                   , _err::Double
                                   , _threshold::Double
                                   , _steps:: Int
                                   }

makeLenses ''KMeansState
