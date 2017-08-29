{-# LANGUAGE MonadComprehensions #-}

module MonadsListsRedux.MonadComprehensions where

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseID = do n <- numberItemsByPurchaseID purchaseID
                                    productID <- productIDByPurchaseID purchaseID
                                    price <- priceByProductID productID
                                    return $ fromInteger n * price


numberItemsByPurchaseID :: Integer -> Maybe Integer
numberItemsByPurchaseID purchaseID = undefined


productIDByPurchaseID :: Integer -> Maybe Integer
productIDByPurchaseID n = undefined


priceByProductID :: Integer -> Maybe Double
priceByProductID n = undefined 

--

-- Using Monad Comprehensions
purchaseValueWithDo2 :: Integer -> Maybe Double
purchaseValueWithDo2 purchaseID = [ fromInteger n * price | n         <- numberItemsByPurchaseID purchaseID,
                                                            productID <- productIDByPurchaseID purchaseID,
                                                            price <- priceByProductID productID ]


-- Using a Guard

                                    

