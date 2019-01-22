
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

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


data GalaxyMessage = LookForGalaxy ProcessId
                   | GalaxyFound String
                   deriving (Typeable, Generic)

data WormholeMessage = LostInWormHole
                       deriving (Typeable, Generic)

instance Binary WormholeMessage

instance Binary GalaxyMessage

traveller :: Process ()
traveller = do LookForGalaxy master <- expect
               -- look for galaxies in space time
               b <- liftIO $ (randomIO::(IO Bool))
               if b
                 then send master (GalaxyFound "Andromeda")
                 else send master LostInWormHole

remotable ['traveller]

master :: [NodeId] -> Process ()
master nodes =
  do myPid <- getSelfPid
     mapM_ (\node -> do pid <- spawn node $(mkStaticClosure 'traveller)
                        send pid (LookForGalaxy myPid))
           nodes
     forever $  do receiveWait [ match $ \(GalaxyFound g) -> say $ "Found Galaxy: " ++ g
                               , match $ \LostInWormHole -> say $ "Lost in Wormhole"
                               ]


main :: IO ()
main = do args <- getArgs
          case args of
            ["master", host, port] -> do backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
                                         startMaster backend master
            ["traveller", host, port] -> do backend <- initializeBackend host port (Main.__remoteTable initRemoteTable)
                                            startSlave backend
            _ -> do putStrLn "Unknown parameters"


                  


