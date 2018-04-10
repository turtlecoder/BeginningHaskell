module Chapter04.Priceable where


class Priceable p where
  price :: p -> Double


-- Excercise 4-4: Prices for the store

totalPrice :: Priceable p => [p] -> Double
totalPrice  pl = foldr (\a b -> (price a) + b) 0.0 pl
