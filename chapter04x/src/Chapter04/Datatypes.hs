{-# LANGUAGE RecordWildCards,InstanceSigs #-}

module Chapter04.Datatypes where

import Chapter04.Typeclasses

data Spec = Forward | Backword deriving Show

data TimeMachineR = TimeMachineR { manufacturer :: String
                                 , model :: Integer
                                 , nameTM :: String
                                 , spec :: Spec
                                 , price :: Double
                                 } deriving Show

data Person = Person { firstName::String,
                       lastName::String }
              deriving (Show, Read)

type AuthorR = Person

data BookR = BookR { priceB :: Double
                   , nameB :: String
                   , authorB :: AuthorR
                   } deriving Show

data TravelGuide = TravelGuide { title :: String
                               , authors :: [String]
                               , priceTG :: Double
                               } deriving (Show, Eq, Ord)

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving Eq

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <= (TravelGuidePrice (TravelGuide t2 a2 p2)) = (p1 < p2) || p1==p2 && (t1<t2 || (t1 == t2) && (a1<=a2))

data Client i = GovOrg { clientID::i,
                         clientName::String
                       }
                | Company { clientID :: i,
                            clientName::String
                          }
                | Individual { clientID :: i,
                               person :: Person
                             }
                  deriving (Show)

data ClientKind = GovOrgKind
                | CompanyKind
                | IndividualKind
                deriving (Show, Eq, Ord)

-- Excercise 4-5
instance Eq Person where
  Person { firstName=fn1, lastName=ln1}  == Person { firstName=fn2, lastName=ln2} = fn1==fn2 && ln1 == ln2

instance Ord Person where
  compare (Person { firstName = fn1, lastName = ln1}) (Person { firstName = fn2, lastName=ln2}) =
    let compareLastNames = ln1 `compare` ln2
        compareFirstNames = fn1 `compare` fn2
    in
      if (compareLastNames == EQ ) then compareLastNames else compareFirstNames
                                                              
instance Eq i =>Eq (Client i) where
  GovOrg { clientID = id1, clientName = cn1} == GovOrg { clientID = id2, clientName = cn2 } = id1 == id2 && cn1 == cn2
  Company { clientID = id1, clientName = cn1 } == Company { clientID = id2 , clientName = cn2} = id1 == id2 && cn1 == cn2
  Individual { clientID = id1, person = p1 } == Individual { clientID = id2, person = p2} = id1 == id2 && p1 == p2

instance Nameable (Client i) where
  name Individual { person = Person { firstName = f, lastName = n}} = f ++ " " ++ n
  name client = (clientName client)


-- Excercise 4-6: Ordering Clients
instance Ord i => Ord (Client i) where
  compare (Individual { person = p1 } ) (Individual { person=p2}) = p1 `compare` p2
  compare (Individual {.. }) _ = LT
  compare _ (Individual { ..} ) = GT
  compare c1 c2 = (clientName c1) `compare` (clientName c2) 
                   

data Complex = C Double Double deriving (Show, Eq)

instance Num Complex where
  (C a1 b1) + (C a2 b2) = C (a1 + a2) (b1 + b2)
  (C a1 b1) - (C a2 b2) = C (a1 - a2) (b1 - b2)
  (C a1 b1) * (C a2 b2) = C (a1 * a2 - b1 * b2) (a1 * b2 + b1 * a2)
  negate (C a b) = C ( negate a) (negate b)
  fromInteger n = C (fromInteger n ) 0
  abs (C a b) = C (sqrt $ a*a + b*b) 0
  signum c @ (C a b) = let C n _ = abs c
                           in
                         C (a/n) (b/n)
                                          
data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1 | Leaf1
  deriving Show

data BinaryTree2 a  = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2
  deriving Show

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c) | Leaf3
  deriving (Show, Eq, Ord)


newtype Min = Min Double deriving Show

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend (Min x) (Min y) = Min $ min x y


modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m = map (\tg -> tg { priceTG = m * priceTG tg })

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { priceTG = m * priceTG  tg })

-- Excercise 4-8: Functor Fun
newtype DTMaybe a = DTMaybe (Maybe a)
instance Functor DTMaybe where
  fmap f (DTMaybe (Just a)) = DTMaybe (Just (f a))
  fmap _ (DTMaybe Nothing) = DTMaybe Nothing

treeInsert2 ::(Ord a)=> a->BinaryTree2 a->BinaryTree2 a 
treeInsert2 a' n@(Node2 a l r) = case a `compare` a' of
                                   EQ -> n
                                   LT -> Node2 a (treeInsert2 a' l) r
                                   GT -> Node2 a l (treeInsert2 a' r)
treeInsert2 a' Leaf2 = Node2 a' Leaf2 Leaf2
      

fmapBinaryTree2 f bt = fmap' f bt Leaf2
    where
      fmap' _ Leaf2 res = res
      fmap' f (Node2 v lt rt) res = let res' = fmap' f lt res
                                        res'' = fmap' f rt res'
                                        res''' = treeInsert2 (f v) Leaf2
                                    in res'''
-- Excercise 4-8:
-- Its not possible to implement a Functor, because functor
-- requires that fmap is implemented for all possible types.
-- and a balanced tree is monadic because it consists of binding
-- two functions (insertion and rebalancing).
-- see http://stackoverflow.com/questions/22899700/defining-fmap-for-a-binary-search-tree
instance Functor BinaryTree2 where
  fmap f (Node2 v lt rt) = Node2 (f v) (fmap f lt) (fmap f rt)
  fmap _ Leaf2 = Leaf2


-- Excercise 4-9
instance Foldable DTMaybe where
  foldr _ z (DTMaybe Nothing) = z
  foldr f z (DTMaybe (Just x)) = f x z

instance Foldable BinaryTree2 where
  foldr _ z Leaf2 = z
  foldr f z (Node2 v lt rt) = foldr f (foldr f z lt) rt
