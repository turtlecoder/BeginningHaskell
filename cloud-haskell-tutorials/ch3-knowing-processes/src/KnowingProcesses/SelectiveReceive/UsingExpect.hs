module KnowingProcesses.SelectiveReceive.UsingExpect (demoIO) where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

demo :: Process ()
demo = do
  listener <- spawnLocal listen
  send listener "hello"
  getSelfPid >>= send listener
  send listener "World!"
  send listener (1::Int)
  () <- expect
  return ()
  where
    listen = do
      -- fourth <- expectTimeout 10000 :: Process (Maybe Int)
      fourth <- expect :: Process Int
      third <- expect :: Process ProcessId
      first <- expect :: Process String
      second <- expectTimeout 10000 :: Process (Maybe String)
      (say . show) first
      (say . show) second
      (say . show) third
      (say . show) fourth
      send third () 

demoIO :: IO ()
demoIO = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node $ demo
