{-# LANGUAGE DeriveDataTypeable, DeriveGeneric,  TemplateHaskell #-}


module Chapter08.CloudHaskell.LookingForGalaxies where

import Data.Binary   (Binary)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)
import Control.Distributed.Process.Closure
import Control.Distributed.Process
import Control.Monad (forever)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node
import System.Environment
import System.Random


data GalaxyMessage = LookForGalaxy ProcessId
                   | GalaxyFound String
                   deriving (Typeable, Generic)

data WormHoleMessage = LostInWormHole
                     deriving (Typeable, Generic)


instance Binary GalaxyMessage
instance Binary WormHoleMessage


traveller::Process ()
traveller = do
  LookForGalaxy m <- expect
  b <- liftIO $ randomIO -- generate random Boolean
  if b
    then send m (GalaxyFound "Andromeda")
    else send m LostInWormHole

remotable ['traveller]               

master :: [NodeId] -> Process ()
master nodes =
  do myPid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     -- forever $ do GalaxyFound g <- expect
     --              say $ "Found galaxy:  " ++ g
     forever $ do receiveWait
                    [ match $ \(GalaxyFound g) -> say $ "Found Galaxy: " ++ g
                    , match $ \LostInWormHole -> say "Lost in Wormhole"
                    ]


mainLookingForGalaxies :: IO ()
mainLookingForGalaxies = do args <- getArgs
                            case args of
                              ["master" , host, port] ->
                                do backend <- initializeBackend host port (__remoteTable initRemoteTable)
                                   startMaster backend master
                              ["traveller", host, port] -> do
                                backend <- initializeBackend host port (__remoteTable initRemoteTable)
                                startSlave backend
                              _ -> do putStrLn "Unknown Parameters"
                                
                                     
