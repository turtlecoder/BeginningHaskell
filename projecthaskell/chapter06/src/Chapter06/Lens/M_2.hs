{-# LANGUAGE TemplateHaskell #-}

module Chapter06.Lens.M_2 where

import Control.Lens
import Data.Char

data Client i = GovOrg { _identifier :: i
                       , _name :: String
                       }
              | Company { _identifier :: i
                        , _name :: String
                        }
              | Individual { _identifier :: i
                           , _person :: Person
                           }

              deriving Show

data Person = Person { _firstName :: String, _lastName :: String }  deriving Show
                           
makeLenses ''Client
makeLenses ''Person


fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\_ newFullName -> case words newFullName of
                                      f:l:_ -> Person f l
                                      _     -> error "Incorrect Name format")

viewClient = let client = Individual 3 (Person "John" "Smith")
             in view (person.lastName) client

viewClientSelector = let client = Individual 4 (Person "Jane" "Doe")
                         in client^.person.firstName

setClient = set identifier 5 client
  where
    client = Individual 4 (Person "John" "Smith")

setClient2 = person.lastName .~ "Kox" $ client
  where
    client = Individual 4 (Person "John" "Smith")

add2ToClient1 = client & identifier +~ 2
  where client = Individual 4 (Person "John" "Smith")


add2ToClient2 = client & over identifier (+2)
  where client = Individual 4 (Person "John" "Smith")

makeClientNamesUpperCase = let client = Individual 4 (Person "John" "Smith")
                           in
                             client & person.fullName %~ (map toUpper)



