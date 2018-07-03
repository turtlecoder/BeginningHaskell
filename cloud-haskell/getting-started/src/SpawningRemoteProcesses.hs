{-# LANGUAGE TemplateHaskell #-}

module SpawningRemoteProcesses(localMain) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)


sampleTask :: (Int, String) -> Process ()
sampleTask (t,s) = liftIO (threadDelay (t*1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = SpawningRemoteProcesses.__remoteTable initRemoteTable

localMain :: IO ()
localMain = do
  Right transport <- createTransport "127.0.0.1" "10501" (\_ -> ("127.0.0.1", "10501")) defaultTCPParameters
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    us <- getSelfNode
    _ <- spawnLocal $ sampleTask (1::Int, "using Spawnlocal")
    pid <- spawn us $ $(mkClosure 'sampleTask) (2::Int, "Using Spawn (remote)")
    liftIO $ threadDelay 4000000
