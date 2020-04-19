{-# LANGUAGE MonadComprehensions #-}

module Chapter07.MonadsAndListsRedux.MonadCompreshensions where

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseId = [ fromInteger n * price | n <- numberItemsByPurchaseId purchaseId
                                                         , productId <- productByPurchaseId purchaseId
                                                         , price <- priceByProductId productId ]


numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId n = undefined


productByPurchaseId :: Integer -> Maybe Integer
productByPurchaseId purchaseId = undefined

priceByProductId :: Integer -> Maybe Double
priceByProductId id = undefined
