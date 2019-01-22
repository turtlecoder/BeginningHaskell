module ArbitraryInstances where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Control.Monad
-- import  Prelude hiding (Maybe)
-- import qualified Prelude as P(Maybe) 

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- Generate a samples of of the trivial data types
-- > sample trivialGen

data Identity a = Identity a
  deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a )

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Sample Some Identity Ints
-- > sample identityGenInt


-- Generators for Pairs
data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen


pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Generator for Pair of ints and strings
-- > sample pairGenIntString


data Sum a b = First a
             | Second b
             deriving (Eq, Show)

-- equal odds for each
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a
        , return $ Second b]
        

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual


newtype PMaybe a = PMaybe (Maybe a) deriving (Show, Eq)

-- arbitrary Instance that produces 1:4 weight for actual values
-- i.e. 25% Nothings, 75% Just values
instance Arbitrary a => Arbitrary (PMaybe a) where
  arbitrary = frequency [(1, return (PMaybe (Nothing))), (3, liftM (\a -> PMaybe (Just a)) arbitrary)]


sumWeightedPMaybe :: (Arbitrary a) => Gen (PMaybe a)
sumWeightedPMaybe = do
  a <- arbitrary
  return a 
  
