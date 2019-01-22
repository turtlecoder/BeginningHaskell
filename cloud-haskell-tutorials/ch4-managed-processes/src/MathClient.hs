module MathClient(mathClientAdd) where

import Control.Distributed.Process (ProcessId
                                   , Process
                                   , whereisRemoteAsync
                                   , WhereIsReply(..)
                                   , liftIO
                                   , NodeId(..)
                                   , expectTimeout
                                   )
import Control.Distributed.Process.ManagedProcess (call)
import Control.Exception.Base (IOException)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import qualified Data.ByteString.Char8 as BS (pack)
import Control.Distributed.Process.Node (newLocalNode, initRemoteTable, runProcess, forkProcess)


import Types

-- Client Code
add :: ProcessId -> Double -> Double -> Process Double
add pid a b = (call pid) $ (Add a b)

mathClientAdd::(HostAddress, Port) -> (HostAddress, Port) -> Double -> Double -> IO (Either IOException ())
mathClientAdd (serverHost, serverPort) (clientHost, clientPort) a b = do
  mt <- createTransport clientHost (show clientPort) (\_ -> (clientHost, (show clientPort))) defaultTCPParameters
  case mt of
    Left err -> do
      print "Client Error Raised (createTransport)" >> print err
      return (Left err :: Either IOException ())
    Right transport -> do
      -- create a node
      putStrLn "Creating a client new node"
      node <- newLocalNode transport initRemoteTable
      putStrLn "Created a new Node"
      runProcess node $ do
        serverPid <- findMathServer (serverHost ++ ":8080:0")
        liftIO.print $ "Client Server PID: " ++ (show serverPid)
        ds <- call serverPid (Add a b) :: Process Double
        liftIO.print $ "Math Add Result: " ++ (show ds)
        return ()
      return (Right () :: Either IOException ())
      
        

-- this is a problim 
findMathServer::HostAddress -> Process ProcessId
findMathServer serverAddress = do
  let addr = EndPointAddress (BS.pack serverAddress)
      nodeId = NodeId addr
  liftIO.print $ "Finding serverPid: " ++ (show nodeId)
  whereisRemoteAsync nodeId "math_server"
  reply <- expectTimeout 100000
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return sid
    _ -> findMathServer serverAddress
  
    
