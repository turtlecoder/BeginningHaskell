{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TimeMachine where

import Control.Lens

data Spec = Forward | Backward deriving Show

data TimeMachine  = TimeMachine { _manufacturer :: String,
                                  _model :: Integer,
                                  _name :: String,
                                  _spec :: Spec,
                                  _price :: Double
                                } deriving Show

makeLenses ''TimeMachine

priceByPercent :: [TimeMachine]->Double->[TimeMachine]
priceByPercent tml p = tml & traversed.price %~ ((p * )) 
