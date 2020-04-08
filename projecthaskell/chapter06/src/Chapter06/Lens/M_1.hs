module Chapter06.Lenses.M_1 where

import Control.Lens

data Client i = GovOrg i String
              | Company i String Person String
              | Individual i Person

data Person = Person String String

firstName :: Simple Lens Person String
firstName = lens (\(Person f _) -> f) (\(Person _ l) newF -> Person newF l)

-- >>> :t firstName
-- firstName
--   :: Functor f => (String -> f String) -> Person -> f Person

lastName :: Simple Lens Person String
lastName = lens (\(Person _ l) -> l) (\(Person f _) newL -> Person f newL)

-- This sample code coverts a Client of type i to a Client of type j
-- I dont understand how this is adding value
identifier :: Lens (Client i) (Client j) i j
identifier =
  lens getClientID setClientID
  where
    getClientID = (\client -> case client of
                              (GovOrg i _) -> i
                              (Company i _ _ _ ) -> i
                              (Individual i _) -> i)
    setClientID = (\client newId -> case client of
                                    (GovOrg _ n) -> GovOrg newId n
                                    (Company _ a b c) -> Company newId a b c
                                    (Individual _ a) -> Individual newId a)

fullName :: Simple Lens Person String
fullName = let
  getFullName (Person f l) = f ++ " " ++ l
  setFullName _  newName = case words newName of
                             f:l:_ -> Person f l
                             _ -> error "Incorrect Name"
  in lens getFullName setFullName
