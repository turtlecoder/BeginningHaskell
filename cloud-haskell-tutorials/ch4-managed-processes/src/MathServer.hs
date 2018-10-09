module MathServer (initMathServer) where

import Prelude
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process (ProcessId
                                   , Process
                                   , whereisRemoteAsync
                                   , expectTimeout)
       
import Control.Distributed.Process.Node (newLocalNode, initRemoteTable, runProcess, forkProcess, localNodeId)
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.ManagedProcess (statelessProcess
                                                  , UnhandledMessagePolicy(Drop)
                                                  , ProcessDefinition(..)
                                                  , serve
                                                  , statelessInit
                                                  , call
                                                  )

import Control.Distributed.Process.ManagedProcess.Server (handleCall_)
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BS (pack)

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))


import Types


launchMathServer :: Process ProcessId
launchMathServer = let server = statelessProcess {
                         apiHandlers = [ handleCall_ (\(Add x y) -> return (x+y)) ]
                         , unhandledMessagePolicy = Drop
                         }
                   in do spawnLocal $ serve () (statelessInit Infinity) server >> return ()
                         

initMathServer :: String -> Int -> IO ()
initMathServer host port = do
  mt <- createTransport host (show port) (\_ -> (host, (show port))) defaultTCPParameters
  print "Created Transport"
  case mt of
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      putStrLn $ "Launching Node: " ++ (show $ localNodeId node)
      runProcess node $ do
        liftIO $ putStrLn "Launch Math Server"
        pid <- launchMathServer
        say $ "Server Process ID: " ++ (show pid)
        register "math_server" pid 
        (liftIO.putStrLn) ("Server Launched at: " ++ (show (nodeAddress.processNodeId $ pid)))
        liftIO $ forever $ threadDelay 1000
    Left  err       -> do
      print "Error Raised"
      print err

