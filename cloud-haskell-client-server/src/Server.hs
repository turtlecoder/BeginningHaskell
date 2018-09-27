{-# LANGUAGE RecordWildCards #-}

module Server where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.ManagedProcess (CastHandler, ActionHandler, serve
                                                  , defaultProcess
                                                  , handleRpcChan
                                                  , handleCast
                                                  , handleInfo
                                                  , InitResult(..)
                                                  , UnhandledMessagePolicy(..)
                                                  , ChannelHandler
                                                  , ProcessDefinition(..))
import Control.Distributed.Process.ManagedProcess (InitResult(..), serve
                                                  , handleRpcChan
                                                  , apiHandlers
                                                  , defaultProcess
                                                  , ChannelHandler)
import Control.Distributed.Process (monitorPort,  PortMonitorNotification(..)
                                   , spawnLocal
                                   ,  Process
                                   , ProcessId(..)
                                   , DiedReason(..)
                                   , sendPortId
                                   , nodeAddress
                                   , register)
import Control.Distributed.Process.ManagedProcess.Server (replyChan, continue)

import Control.Distributed.Process.Extras.Time (Delay(..))

import Control.Distributed.Process.Node (initRemoteTable
                                        , runProcess
                                        , newLocalNode
                                        )


import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, forM_, void)
import Control.Concurrent (threadDelay)
import qualified Data.Map as M (insert, empty, member, delete, filter, elemAt)
import Types
import Logger

serveChatRoom :: Host -> Int -> ChatName -> IO ()
serveChatRoom host port name = do
  let serviceName = "service_chat_room"
  mt <- createTransport host (show port) (\ _  -> (host, serviceName)) defaultTCPParameters
  print "Created Transport"
  case mt of
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      print "Created Local Node"
      runChatLogger node
      runProcess node $ do
        (liftIO.putStrLn) "Launching Chat Server"
        pId <- launchChatServer
        logStr $ "Server Launched at: " ++ show (nodeAddress . processNodeId $ pId)
        register name pId
        liftIO $ forever $ threadDelay 500000
    Left err -> print err
        

broadcastMessage :: ClientPortMap -> ChatMessage -> Process () 
broadcastMessage clientPorts msg = forM_ clientPorts (`replyChan` msg)

messageHandler :: CastHandler ClientPortMap ChatMessage
messageHandler = handler
  where handler :: ActionHandler ClientPortMap ChatMessage
        handler clients msg = do
          broadcastMessage clients msg
          continue clients

joinChatHandler :: ChannelHandler ClientPortMap JoinChatMessage ChatMessage
joinChatHandler sendPort = handler
  where
    handler :: ActionHandler ClientPortMap JoinChatMessage
    handler clients JoinChatMessage{..} =
      if clientName `M.member` clients
      then replyChan sendPort (ChatMessage Server "Nickname already in use ... ") >> continue clients
      else do
        void $ monitorPort sendPort
        let clients' = M.insert clientName sendPort clients
            msg = clientName ++ " has joined the chat ..."
        logStr msg
        broadcastMessage clients $ ChatMessage Server msg
        continue clients'
        

disconnectHandler :: ActionHandler ClientPortMap PortMonitorNotification
disconnectHandler clients (PortMonitorNotification _ spId reason) = do
  let search = M.filter (\v -> sendPortId v == spId) clients
  case (null search, reason ) of
    (False, DiedDisconnect) -> do
      let (clientName, _) = M.elemAt 0 search
          clients' = M.delete clientName clients
      broadcastMessage clients' (ChatMessage Server $ clientName ++ " has left the chat ...")
      continue clients'
    _ -> continue clients
    
launchChatServer :: Process ProcessId
launchChatServer = let server = defaultProcess { apiHandlers = [handleRpcChan joinChatHandler]
                                               , infoHandlers = [ handleInfo disconnectHandler ]
                                               , unhandledMessagePolicy = Log
                                               }
                   in
                     do
                       (liftIO.putStrLn) "Spawning Local"
                       spawnLocal $ serve () (const (return $ InitOk M.empty Infinity)) server >> return ()
                     


