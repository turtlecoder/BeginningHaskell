module IncompleteData where

import Data.Maybe

meanPurchase :: Integer -> Double
meanPurchase clientID = let p = purchasesByClientID clientID
                        in foldr (+) 0.0 $ catMaybes $ map purchaseValue p


purchasesByClientID::Integer -> [Integer]
purchasesByClientID clientID = undefined

purchaseValue::Integer->Maybe Double
purchaseValue purchaseID = case numberItemsPurchasedBy purchaseID of
                    Nothing -> Nothing
                    Just n -> case productIDByPurchaseID purchaseID of
                                Nothing -> Nothing
                                Just productID -> case priceByProductID productID of
                                                    Nothing -> Nothing
                                                    Just price -> Just $ (fromInteger n) * price

numberItemsPurchasedBy :: Integer -> Maybe Integer
numberItemsPurchasedBy id = undefined

productIDByPurchaseID :: Integer -> Maybe Integer
productIDByPurchaseID id = undefined


priceByProductID :: Integer -> Maybe Double
priceByProductID id = undefined

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just a) f = f a


purchaseValue2::Integer -> Maybe Double
purchaseValue2 purchaseID =
  numberItemsPurchasedBy purchaseID `thenDo`
  (\n -> productIDByPurchaseID purchaseID `thenDo`
    (\productID -> priceByProductID productID `thenDo`
      (\price -> Just $ fromInteger n * price)))
                                                

purchaseValue3 :: Integer -> Maybe Double
purchaseValue3 purchaseID =
  numberItemsPurchasedBy purchaseID >>=
  (\n -> productIDByPurchaseID purchaseID >>=
  (\productID -> priceByProductID productID >>=
  (\price -> Just $ fromInteger n * price)))

