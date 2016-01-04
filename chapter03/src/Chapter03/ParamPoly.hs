{-# LANGUAGE ExplicitForAll #-}

module Chapter03.ParamPoly where

maybeString::forall t .Maybe t -> String
maybeString (Just _ ) = "Just"
maybeString Nothing = "Nothing"

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

data Triple a b c = Triple a b c
                    deriving Show

data SamePair a = SamePair a a

swapTriple :: (a,b,c) -> (b, c, a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a,a)
duplicate x = (x,x)

nothing:: a -> Maybe a
nothing _ = Nothing

index :: [a] -> [(Int, a)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
                   in (n+1, x):indexed

maybeA::[a] -> Char 
maybeA [] = 'a'


