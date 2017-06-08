{-# LANGUAGE NoImplicitPrelude #-}

module IncompleteData65 where

import Prelude (Maybe(Just, Nothing)
               , String
               , error
               , Integer, Double, foldr, (+), map, undefined, ($), (*)
               , fromInteger)

import Data.Maybe (catMaybes)

class Monad m where

  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  f >> g = f >>= (\ _ -> g)

  fail :: String -> m a
  fail s = error s
  

instance Monad Maybe where
  return a = Just a
  a >>= fg = case a of
               Just a -> fg a
               Nothing -> Nothing 



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

