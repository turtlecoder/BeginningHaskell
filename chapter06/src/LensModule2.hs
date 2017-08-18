{-# LANGUAGE TemplateHaskell #-}

module LensModule2 where

import Control.Lens

data Client i = GovOrg { _identifier :: i, _name :: String }
              | Company { _identifier :: i,
                          _name :: String ,
                          _person :: Person,
                          _duty :: String
                        }
              | Individual { _identifier :: i, _person :: Person }
              deriving Show

data Person = Person {
  _firstName :: String,
  _lastName :: String
  }
  deriving Show

makeLenses ''Client
makeLenses ''Person

fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                     f:l:_ -> Person f l
                                     _     -> error "Incorrect name")


p = let p = Person "John" "Smith"
    in (view firstName p, p^.lastName)

client = Individual 3 (Person "John" "Smith")

clientlastName = view (person.lastName) client

clientFullName = client^.person.fullName

setIdentifier4 = set identifier 4 client

koxClient = person.lastName .~ "Kox" $ client

marrianeKoxClient = person.fullName .~ "Marriane Kox" $ client

clientIdentifier2 = client & identifier +~ 2
