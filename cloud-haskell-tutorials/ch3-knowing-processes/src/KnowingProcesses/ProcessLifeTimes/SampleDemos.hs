module KnowingProcesses.ProcessLifeTimes.SampleDemos where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess, closeLocalNode)
import Control.Distributed.Process.Backend.SimpleLocalnet

-- this will never print anything ...
demo1 = die "Boom" >> expect >>= say



runDemo1 = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node demo1
  closeLocalNode node

-- this might print something before it prints
demo2 = do
  self <- getSelfPid
  exit self "Boom"
  expect >>= say

runDemo2 = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node demo2
  closeLocalNode node
  
