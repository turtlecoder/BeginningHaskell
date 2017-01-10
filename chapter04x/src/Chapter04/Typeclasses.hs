{-# LANGUAGE LambdaCase #-}

module Chapter04.Typeclasses where

class Nameable n where
  name :: n -> String

class Priceable p where
  getPrice::p -> Double
