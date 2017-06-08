module DiscoverMonads where

import Data.Maybe

meanPurchase :: Integer  -- the client identifier
             -> Double -- mean purchase
meanPurchase clientID = let p = purchasesByClientID clientID
                        in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchasesByClientID :: Integer -> [Integer]
purchasesByClientID id = [1,2,3]

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseID = Nothing
