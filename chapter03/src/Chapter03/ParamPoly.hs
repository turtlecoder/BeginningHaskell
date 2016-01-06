{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}


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

map2 _ [] = []
map2 f (x:xs) = (f x) : (map f xs)

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f ( x + 2 )

($$) :: (a->b) -> a -> b
f $$ a = f a

equalTuples::[(Integer, Integer)]->[Bool]
equalTuples t = map (\(x,y)->x==y) t

sayHello::[String] -> [String]
sayHello names = map ( \name -> case name of
                         "Alejandro" -> "Hello, Writer"
                         _           -> "Welcome, " ++ name ) names


sayHello2::[String] -> [String]
sayHello2 names = map (\case "Alejandro" -> "Hello, writer"
                             name        -> "Welcome, " ++ name
                             ) names
