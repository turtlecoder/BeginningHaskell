module KnowingProcesses.MonitoringAndLinking where


import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet 

linkDemo = do
  say "Hello, from linkDemo parent"
  pid <- spawnLocal $ do
    say "Hello, from linkDemo child"
    () <- expect
    say "Child Process Done"
  link pid
  send pid ()
  () <- expect
  return ()

runLinkDemo = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node linkDemo
  
