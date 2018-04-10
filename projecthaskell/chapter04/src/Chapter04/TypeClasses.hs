module Chapter04.TypeClasses where

import Chapter04.Client

class Nameable n where
  name :: n -> String



initial :: Nameable n => n -> Char
initial n = head (name n)

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n} } = f ++ " " ++ n
  name c = clientName c

-- Excercise 4-4: Prices for the store
class Priceable p where
  getPrice :: p -> Double
