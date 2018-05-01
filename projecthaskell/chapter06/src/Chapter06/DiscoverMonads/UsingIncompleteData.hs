module Chapter06.DiscoverMonads where

import Data.Maybe



meanPurchase :: id -> Double
meanPurchase clientId = let purchaseIds = purchasesByClientId clientId
                            in foldr (+) 0.0 $ catMaybes $ map purchaseValue purchaseIds

purchasesByClientId :: clientId -> [purchaseId]
purchasesByClientId clientId = undefined

numberOfItemsByPurchaseId:: purchaseId -> Maybe n
numberOfItemsByPurchaseId purchaseId = undefined

purchaseValue :: purchaseId -> Maybe Double
purchaseValue purchaseId = case numberOfItemsByPurchaseId purchaseId of
                             Nothing -> Nothing
                             Just n -> case productIdByPurchaseId purchaseId of
                                         Nothing -> Nothing
                                         Just productId -> case priceByProductId productId of
                                                             Nothing -> Nothing
                                                             Just price -> Just $ (fromInteger n) * price

  
productIdByPurchaseId::purchaseId -> Maybe productId
productIdByPurchaseId purchaseId = undefined

priceByProductId :: productId -> Maybe Double
priceByProductId _ = undefined
