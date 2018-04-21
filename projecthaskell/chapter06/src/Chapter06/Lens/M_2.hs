{-# LANGUAGE TemplateHaskell #-}

module Chapter06.Lens.M_2 where

import Control.Lens

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

fullName = let p = Person "John" "Smith"
           in
             (view firstName p, p^.lastName)

viewClient = let client = Individual 3 (Person "John" "Smith")
            in view (person.lastName) client

viewClientSelector = let client = Individual 4 (Person "Jane" "Doe")
                         in client^.person.firstName
