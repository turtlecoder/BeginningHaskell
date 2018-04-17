{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Lens

import LensModule2

data Spec = Forward | Backward deriving Show

data TimeMachine = TimeMachine { _manufacturer :: String
                              , _model :: Integer
                              , _name :: String
                              , _spec :: Spec
                              , _price :: Double
                              } deriving Show

-- data TravelGuide = TravelGuide { _title :: String
--                                , _authors :: [Person]
--                                , _price :: Double
--                                } deriving Show

makeLenses ''TimeMachine



adjustPrice :: Double -> [TimeMachine] -> [TimeMachine]
adjustPrice percent tms= tms & traversed.price %~ ((1+percent)*)


meanPurchase :: Integer      -- the client identifier
             -> Maybe Double -- the mean purchase
meanPurchase clientID = undefined

numberItemsByPurchaseID :: Integer -> Maybe Integer
numberItemsByPurchaseID id = undefined

priceByProductID :: Integer -> Maybe Double
priceByProductID id = undefined

productIDByPurchaseID :: Integer -> Maybe Integer
productIDByPurchaseID id = undefined

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseID =
  case numberItemsByPurchaseID purchaseID of
    Nothing -> Nothing
    Just n -> case productIDByPurchaseID purchaseID of
                Nothing -> Nothing
                Just productID -> case priceByProductID productID of
                                    Nothing -> Nothing
                                    Just price -> Just $ (fromInteger n) * price

                                                    
thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x

purchaseValue2 purchaseID =
  numberItemsByPurchaseID purchaseID `thenDo`
  (\n -> productIDByPurchaseID purchaseID `thenDo`
    (\productID -> priceByProductID productID `thenDo`
      (\price -> Just $ fromInteger n * price )))



purchaseValue3 purchaseID =
  numberItemsByPurchaseID purchaseID >>=
  (\n -> productIDByPurchaseID purchaseID >>=
    (\productID -> priceByProductID productID >>=
      (\price -> Just $ fromInteger n * price )))
                                              
purchaseValueWithDo :: Integer -> Maybe Double
purchaseValueWithDo purchaseID = do n <- numberItemsByPurchaseID purchaseID
                                    productID <- productIDByPurchaseID purchaseID
                                    price <- priceByProductID productID
                                    return $ (fromInteger n) * price




