{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module MathServer (launchMathServer) where

import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process (ProcessId, Process)
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.ManagedProcess (statelessProcess
                                                  , UnhandledMessagePolicy(Drop)
                                                  , ProcessDefinition(..)
                                                  , serve
                                                  , statelessInit
                                                  , call
                                                  )
import Control.Distributed.Process.ManagedProcess.Server (handleCall_)
import Data.Binary
import Data.Typeable
import GHC.Generics


data Add = Add Double Double
  deriving (Generic, Typeable, Eq, Show)

instance Binary Add 

-- Client Code
add :: ProcessId -> Double -> Double -> Process Double
add pid a b = call pid (Add a b)


launchMathServer :: Process ProcessId
launchMathServer = let server = statelessProcess {
                         apiHandlers = [ handleCall_ (\(Add x y) -> return (x+y)) ]
                         , unhandledMessagePolicy = Drop
                         }
                   in spawnLocal $ serve () (statelessInit Infinity) server >> return ()
