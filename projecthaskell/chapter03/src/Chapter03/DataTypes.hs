module Chapter03.DataTypes where

data Client i = GovOrg { clientID :: i
                       , clientName :: String
                       }
                | Company { clientID :: i
                          , clientName :: String
                          , person :: Person
                          , duty :: String
                          }
                | Individual { clientID :: i
                             , person :: Person
                             }
                  deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     } deriving (Show, Eq, Ord)


data Triple a b c = Triple a b c 
