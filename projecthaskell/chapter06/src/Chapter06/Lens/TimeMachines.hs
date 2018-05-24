{-# LANGUAGE TemplateHaskell #-}

module Chapter06.Lenses.TimeMachines where

import Control.Lens


data TimeMachine = TimeMachine { _manufacturer :: String
                               , _model :: Integer
                               , _name :: String
                               , _direction :: Direction
                               , _price :: Price
                               } deriving Show


data Direction = Backward | Forward deriving Show

type Price = Double

makeLenses ''TimeMachine


-- Excercise 6-2
updatePrice :: [TimeMachine] -> Double -> [TimeMachine]
updatePrice lst percent = lst & traversed.price %~ (* (1.0 + percent))
