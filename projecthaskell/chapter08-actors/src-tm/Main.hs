{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment
import Control.Monad (forever)
import System.Random
import Data.Set


main :: IO ()
main = do putStrLn "Hello, World from the Time Machine Store"

data TimeJump = JumpToYear ProcessId Int
              | Wait
              | Travelling Int
              | Arrived Int
              | LookForStore ProcessId
              deriving (Typeable, Generic)

instance Binary TimeJump

timeTravelService :: Set Int -> Process ()
timeTravelService yearsUsed =
  do storePid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $ (mkStaticClosure 'traveller)
                        send pid (LookForStore storePid))
       nodes
     receiveWait
       [ match $ \(JumpToYear travellerProcId year) ->
                   if year `member` yearsUsed
                   then do _ <- send travellerProcId Wait
                           timeTravelService yearsUsed
                   else do _ <- send  travellerProcId (Travelling 100)
                           timeTravelService (insert year yearsUsed)
       , match $ \(Arrived year) -> timeTravelService (delete year yearsUsed)]


-- traveller :: Int -> Process ()
-- traveller year = do storePid <- expect
