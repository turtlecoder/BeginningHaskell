{-# LANGUAGE ScopedTypeVariables #-}
module KnowingProcesses.SelectiveReceive.UsingReceive where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

demo :: Process ()
demo = do
  listener <- spawnLocal listen
  send listener "hello"
  thisPid <- getSelfPid
  send listener thisPid
  send listener "World!"
  send listener (1::Int)
  () <- expect
  return ()
  where
    listen = do 
      receiveWait [ match (\(s::String) -> say $ show s)
                  , match (\(pid::(ProcessId)) -> (say $ show pid) >> send pid ())
                  , match (\(n::Int) -> say $ show n)
                  ]
      listen -- reprocess messages once they are received
      
        

demoIO :: IO ()
demoIO = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node $ demo
