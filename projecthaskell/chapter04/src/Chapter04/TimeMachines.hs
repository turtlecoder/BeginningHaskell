module Chapter04.TimeMachines where

import Chapter04.Priceable

data TimeMachine = TimeMachine { _manufacturer :: Manufacturer
                               , _model :: Integer
                               , _name :: String
                               , _direction :: Direction
                               , _price :: Price
                               } deriving Show

type Manufacturer = String

type Model = Integer

data Direction = Forward
               | Backward
               deriving (Show)

type Price = Double

instance Priceable TimeMachine where
  price tm = _price tm

