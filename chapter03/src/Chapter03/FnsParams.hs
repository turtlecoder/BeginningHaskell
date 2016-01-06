{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}

module Chapter03.FnsParams where

data Client i = GovOrg { clientId :: i
                       , clientName :: String
                       }
              | Company { clientId :: i
                        , clientName :: String
                        , person :: Person
                        , duty :: String
                        }
              | Individual { clientId :: i
                           , person :: Person
                           }
              deriving (Show, Eq, Ord)

data Person = Person { firstName :: String
                     , lastName :: String
                     }
              deriving (Show, Eq, Ord)


filterOnes xs = filter (\x -> x==1) xs

filterANumber n xs = filter (\x -> x == n ) xs

filterNot f xs = filter (\x -> not(f x)) xs

filterGovOrgs clients = filter (\aClient -> case aClient of
                                   GovOrg _ _ -> True
                                   _        -> False ) clients

filterGovOrgsUsingLambdaCase clients = filter (\case GovOrg _ _ -> True
                                                     _          -> False) clients
(***) :: (a->b)->(c->d) -> ((a,c) -> (b,d))
f *** g = \(x,y) -> (f x, g y)
