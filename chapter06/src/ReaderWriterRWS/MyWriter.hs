{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ReaderWriterRWS.MyWriter where

data MyWriter w a = MyWriter (a,w)

-- Excercise 6-6 

instance (Monoid w, Applicative (MyWriter w)) => Monad (MyWriter w) where
  return x = MyWriter (x,mempty) 
  (MyWriter (a,w)) >>= f = f a


