{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module CoArbitrarySample where


import Control.Applicative
import Control.Monad
import System.Random

newtype Gen a = Gen { unGen :: StdGen -> a }

class Arbitrary a where
  arbitrary :: Gen a

instance Arbitrary Bool where
  arbitrary = Gen $ \stdgen -> let (r, _) = randomR (False, True) stdgen in r

genBoolFn0 :: Gen (Bool -> Bool)
genBoolFn0 = Gen $ \stdgen -> \ _  -> let (r, _) = randomR (False, True) stdgen in r

genBoolFn1 :: Gen (Bool -> Bool)
genBoolFn1 = Gen $ (\stdgen -> \a -> case a of
                       True -> let (r, _) = randomR (False, True) stdgen in r
                       False -> let (r, _) = randomR (False, True) stdgen in r)


genBoolFn2 :: Gen (Bool -> Bool)
genBoolFn2 = Gen $ \stdgen -> \a -> case a of
  True -> let (r, _) = randomR (False, True) (splitGenerator stdgen !! 0 ) in r 
  False -> let (r, _) = randomR (False, True) (splitGenerator stdgen !! 1) in r

runBoolFnGen :: Gen (Bool -> Bool) -> IO ()
runBoolFnGen g = do
  fns <- samples g

  forM_ fns $ \f -> do
    putStrLn $ "True => " ++ show (f True)
    putStrLn $ "False => " ++ show (f False)
    putStrLn ""

splitGenerator :: RandomGen a => a -> [a]
splitGenerator r = r0 : splitGenerator r1
  where
    (r0, r1) = split r 

samples :: Gen a -> IO [a]
samples g = generate $ sequence [g, g, g, g, g, g, g, g, g, g, g]

generate :: Gen a -> IO a
generate (Gen g) = do
  stdgen <- getStdGen
  return $ g stdgen


instance Monad Gen where
  return :: a -> Gen a
  return a = Gen $ \_ -> a

  (>>=) :: Gen a -> (a -> Gen b) -> Gen b
  (Gen g) >>= f = Gen $ \r -> let (r1, r2) = split r
                                  Gen k = f $ g r1
                              in k r2

instance Functor Gen where
  fmap f (Gen h) = Gen $ \r -> f (h r )


instance Applicative Gen where
  pure x = Gen $ \_ -> x

  f <*> x = do
    f' <- f
    x' <- x
    return $ f' x'


variant :: Int -> Gen b -> Gen b
variant v (Gen g) = Gen $ \r -> g $ splitGenerator r !! (v+1)

class CoArbitrary a where
  coarbitrary :: a -> Gen b -> Gen b

instance CoArbitrary Bool where
  coarbitrary False = variant 0
  coarbitrary True = variant 1

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary [] = variant 0
  coarbitrary (x:xs) = variant 1 . coarbitrary (x,xs)

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a,b) where
  coarbitrary (x, y) = coarbitrary x . coarbitrary y

instance (CoArbitrary a, Arbitrary b) => Arbitrary (a->b) where
  arbitrary = promote (\a -> coarbitrary a arbitrary)

promote :: (a -> Gen b) -> (Gen (a ->b))
promote f = Gen $ \r ->
  \a -> let Gen h = f a
        in h r 

runGenFn :: (Arbitrary a, Arbitrary b, Show a , Show b)
  => Gen (a -> b) -> [a] -> IO ()
runGenFn g as = do
  fns <- samples g
  forM_ fns $ \f -> do
    forM_ as $ \a -> putStrLn $ show a ++ " => " ++ show (f a )
      
  
instance Arbitrary Int where
  arbitrary = Gen $ \stdgen -> let (r, _) = randomR (-1000, 1000) stdgen in r

instance CoArbitrary Int where
  coarbitrary n = variant $ if n >= 0 then 2*n else 2*(-n)+1

instance Arbitrary a => Arbitrary [a] where
  arbitrary = listOf arbitrary


listOf :: Gen a -> Gen [a]
listOf g = do
  k <- choose (1, 10)
  vectorOf k g

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf = replicateM

choose :: Random a =>  (a,a) -> Gen a
choose range = Gen $ \stdgen -> let (r, _) = randomR range stdgen in r 
