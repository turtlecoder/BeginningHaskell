{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Chapter08.CloudHaskell.TimeMachineStore where

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Control.Distributed.Process.Closure
import Control.Distributed.Process

import Control.Monad (replicateM)
import Control.Distributed.Process.Backend.SimpleLocalnet 
import Control.Distributed.Process.Node hiding (newLocalNode)
import System.Random



data TimeMachineMessage = TimeMachineStoreReady ProcessId
                        | TeleportYear Int ProcessId
                        | TeleportDone 
                        deriving (Typeable, Generic)

instance Binary TimeMachineMessage

traveller :: Process ()
traveller = do
  TimeMachineStoreReady storePid <- expect
  myPid <- getSelfPid
  year <- liftIO $ (randomRIO (2000, 2018) :: IO Int)
  send storePid (TeleportYear year myPid)
  TeleportDone <- expect
  say $ "I " ++ (show myPid) ++ " travelled to " ++ (show year)
  return ()

remotable ['traveller]


timeMachineStore :: [NodeId] -> Process()
timeMachineStore nodes =
  do myPid <- getSelfPid
     -- create 10 travellers on every node
     travellerPids' <- mapM (\node -> replicateM 10 $ do travellerPid <- spawn node $(mkStaticClosure 'traveller)
                                                         send travellerPid (TimeMachineStoreReady myPid)
                                                         return travellerPid)
                      nodes
     let travellerPids = concatMap (\al -> al) travellerPids'
     -- Monitor the traveller pids
     _  <- mapM (\pid -> monitor pid) travellerPids
     handleTravellerMessages 10
     mapM_ (\node -> terminateSlave node) nodes

handleTravellerMessages :: Int -> Process ()
handleTravellerMessages pidCount =
  if (pidCount > 0 )
  then receiveWait [
    match $ (\(TeleportYear year pid) -> do
                say $ "sending traveller " ++ (show pid) ++ " to year " ++ (show year)
                send pid TeleportDone
                handleTravellerMessages pidCount
            ),
      match $ (\(ProcessMonitorNotification _ pid diedReason) -> do
                  say $ "traveller ("++ (show pid) ++ ") died because of " ++ (show diedReason)
                  handleTravellerMessages (pidCount-1))
      ]
  else return ()
   
    
                                      

mainTimeMachineStore :: [[Char]] -> IO ()
mainTimeMachineStore args = do
  case args of
    ["store", host, port] ->
      do backend <- initializeBackend host port (__remoteTable initRemoteTable)
         startMaster backend timeMachineStore
    "traveller": host : port : [] ->
      do backend <- initializeBackend host port (__remoteTable initRemoteTable)
         startSlave backend
         -- let Backend { newLocalNode = mkLocalNode } = backend
         -- localNode <- mkLocalNode
         -- replicateM_  10 $ (runProcess localNode traveller)
         -- return ()
    _ -> do putStrLn "Unknown Parameters"
