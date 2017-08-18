{-# LANGUAGE TemplateHaskell #-}
module KMeansDataLens where

import Control.Lens

data KMeansState v = KMeansData { _centroids :: [v]
                                , _threshold :: Double
                                , _steps :: Int
                                }

                     
makeLenses ''KMeansState
