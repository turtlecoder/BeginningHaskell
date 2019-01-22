{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Binary
import Data.Typeable
import GHC.Generics

data Add = Add Double Double
  deriving (Generic, Typeable, Eq, Show)

instance Binary Add

type HostAddress = String

type Port = Int


