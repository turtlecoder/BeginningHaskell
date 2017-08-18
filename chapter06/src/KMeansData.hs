module KMeansData where

data KMeansState v = KMeansState { centroids :: [v]
                                 , threshold :: Double
                                 , steps :: Int }
