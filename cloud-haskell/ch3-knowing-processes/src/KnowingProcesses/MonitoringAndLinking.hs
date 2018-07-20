module KnowingProcesses.MonitoringAndLinking where


import Control.Distributed.Process
import qualified Control.Distributed.Process as P
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet 
import Control.Concurrent
import Control.Monad (void)

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
  
linkOnFailure them = do
  say "Linking on Failure"
  us <- getSelfPid
  tid <- liftIO $ myThreadId
  -- spawn the monitor process
  void $ spawnLocal $ do
    say "Launching Monitor Process"
    callerRef <- P.monitor us
    say ("callerRef" ++ show callerRef)
    calleeRef <- P.monitor them
    say ("calleeRef" ++ show callerRef)
    reason <- receiveWait [
      matchIf (\(ProcessMonitorNotification mRef _ _  ) -> mRef == callerRef)
        (\ _ -> do
            say "Died Normal"
            return DiedNormal)
      , matchIf (\(ProcessMonitorNotification mRef' _ _ ) -> mRef' == calleeRef)
        (\(ProcessMonitorNotification _ _ r') -> do
            say $ show r'
            return r')
      ]
    say "Got a reason"
    case reason of
      DiedNormal -> return ()
      _ -> liftIO $ throwTo tid (ProcessLinkException us reason)

childProcess = do
  say "Hello, from child process"
  expectTimeout 100000 :: Process (Maybe ())
  say "Bye, from child process"
  pid <- getSelfPid
  kill pid "Die Die Die!!!"
  return ()

parentProcess = do
    say "Hello, from Parent process"
    them <- spawnLocal childProcess
    linkOnFailure them
    expectTimeout 1000000 :: Process (Maybe ())
    say "Bye, from parent process"

demoLinkOnFailure = do
  backend <- initializeBackend "127.0.0.1" "10501" initRemoteTable
  node <- newLocalNode backend
  runProcess node $ parentProcess
    
