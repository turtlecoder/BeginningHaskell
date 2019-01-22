{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Map
import Control.Distributed.Process (SendPort)
import Data.Binary
import Data.Typeable
import GHC.Generics

type Host = String

type ChatName = String

type NickName = String

type ClientPortMap = Map NickName (SendPort ChatMessage)

data Sender = Server | Client NickName
            deriving (Generic, Typeable, Eq, Show)

type ServerAddress = String

instance Binary Sender

data ChatMessage = ChatMessage { from :: Sender
                               , message :: String
                               } deriving (Generic, Typeable, Show )

instance Binary ChatMessage

data JoinChatMessage = JoinChatMessage {
  clientName :: NickName
  } deriving (Generic, Typeable, Show)

instance Binary JoinChatMessage



