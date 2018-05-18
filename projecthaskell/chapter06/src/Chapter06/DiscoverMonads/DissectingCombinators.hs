{-# LANGUAGE NoImplicitPrelude #-}

module Chapter06.DiscoverMonads.DissectingCombinators where


import Prelude (map, ($), (+), foldr, undefined, Double, Maybe(..), String, error)
import Data.Maybe (catMaybes)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  f >> g = f >>= (\_ -> g)
  fail :: String -> m a
  fail s = error s
  
-- Excercise 6-5 Implement local monad instance for Maybe

instance Monad Maybe where
  return a = Just a
  (Just a) >>= f = f a
  Nothing >>= _ = Nothing

meanPurchase :: id -> Double
meanPurchase clientId = let purchaseIds = purchasesByClientId clientId
                        in foldr (+) 0.0 $ catMaybes $ map purchaseValue purchaseIds

purchasesByClientId :: clientId -> [purchaseId]
purchasesByClientId clientId = undefined

purchaseValue :: purchaseId -> Maybe Double
purchaseValue purchaseId =
  (numberOfItemsByPurchaseId purchaseId) >>=
  (\n -> productIdByPurchaseId purchaseId >>=
    (\productId -> priceByProductId productId >>=
      (\price -> undefined)))

numberOfItemsByPurchaseId :: purchaseId -> Maybe n
numberOfItemsByPurchaseId purchaseId = undefined

productIdByPurchaseId::purchaseId -> Maybe productId
productIdByPurchaseId purchaseId = undefined

priceByProductId :: productId -> Maybe Double
priceByProductId _ = undefined
