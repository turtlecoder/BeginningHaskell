{-# LANGUAGE NoImplicitPrelude #-}

import Prelude ( (+)
               , return
               , ($)
               , sqrt
               , Maybe(Just, Nothing)
               , Float
               , (>>=)
               , Integer
               , Double
               , foldr
               , undefined
               , fromInteger
               , (*))
import Data.Maybe (catMaybes)
import Data.Map.Lazy (map)

obtainNumber1 :: Maybe Float
obtainNumber1 = Just 1

obtainNumber2 :: Maybe Float
obtainNumber2 = Just 2

foo1 =
  do number1 <- obtainNumber1
     number2 <- obtainNumber2
     let sum = number1 + number2
     return $ sqrt sum

foo2 =
  obtainNumber1 >>=
  (\n1 -> obtainNumber2 >>=
  (\n2 -> let sum = n1 + n2
    in return $ sqrt sum))

foo3 =
  obtainNumber1 >>=
  (\n1 -> obtainNumber2 >>=
  (\n2 -> return $ sqrt (n1 + n2)))

-- meanPurchase :: Integer -> Double
-- meanPurchase clientID = let p = purchasesByClientID clientID
--                         in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchaseValue::Integer->Maybe Double
purchaseValue purchaseID = case numberItemsPurchasedBy purchaseID of
                    Nothing -> Nothing
                    Just n -> case productIDByPurchaseID purchaseID of
                                Nothing -> Nothing
                                Just productID -> case priceByProductID productID of
                                                    Nothing -> Nothing
                                                    Just price -> Just $ (fromInteger n) * price


purchasesByClientID::Integer -> [Integer]
purchasesByClientID clientID = undefined


numberItemsPurchasedBy :: Integer -> Maybe Integer
numberItemsPurchasedBy id = undefined

productIDByPurchaseID :: Integer -> Maybe Integer
productIDByPurchaseID id = undefined


priceByProductID :: Integer -> Maybe Double
priceByProductID id = undefined

purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseID = do n <- numberItemsPurchasedBy purchaseID
                                    productID <- productIDByPurchaseID purchaseID
                                    price <- priceByProductID productID
                                    return $ fromInteger n * price
