module Client where

import Control.Distributed.Process.ManagedProcess.Client (callChan, cast)
import Control.Distributed.Process ( expectTimeout
                                   , whereisRemoteAsync
                                   , spawnLocal
                                   , receiveChan
                                   , link
                                   , NodeId(..)
                                   , Process
                                   , ProcessId
                                   , ReceivePort
                                   , WhereIsReply(..)
                                   , say)

import Control.Distributed.Process.Node ( initRemoteTable
                                        , runProcess
                                        , newLocalNode)

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (EndPointAddress(..))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forever)
import qualified Data.ByteString.Char8 as BS (pack)
import Types
import Logger (runChatLogger, logChatMessage, logStr)

searchChatServer :: ChatName -> ServerAddress -> Process ProcessId
searchChatServer chatRoom serverAddr = do
  say $ "searchChatserver " ++ chatRoom ++ " " ++ serverAddr
  let addr = EndPointAddress (BS.pack serverAddr)
      nodeId = NodeId addr
  whereisRemoteAsync nodeId chatRoom
  reply <- expectTimeout 10000
  case reply of
    Just (WhereIsReply _ (Just sid)) -> return sid
    _ -> searchChatServer chatRoom serverAddr

launchChatClient :: (ServerAddress, Int) -> (Host, Int) -> ChatName -> IO ()
launchChatClient (serverAddr,sport) (clientHost, cport) name = do
  mt <- createTransport clientHost (show cport) (\_ -> (clientHost, show $ cport)) defaultTCPParameters
  case mt of
    Left err -> print err
    Right transport -> do
      node <- newLocalNode transport initRemoteTable
      runChatLogger node
      runProcess node $ do
        serverPid <- searchChatServer name (serverAddr ++ ":" ++ (show $ sport) ++ ":0")
        say $ "Found a server PID: " ++ (show serverPid)
        link serverPid
        logStr "Joining Chat Server"
        logStr "Please provide your nickname..."
        nickName <- liftIO getLine
        rp <- callChan serverPid (JoinChatMessage nickName) :: Process (ReceivePort ChatMessage)
        logStr "Client You have joined the chat ..."
        void $ spawnLocal $ forever $ do
          say "Logging Chat Message"
          msg <- receiveChan rp
          logChatMessage msg
        say "Starting Client Prompt"
        forever $ do
          chatInput <- liftIO getLine
          say $ "got Chat Input: " ++ chatInput
          cast serverPid (ChatMessage (Client nickName) chatInput)
          say $ "Sent chat input:" ++ chatInput
          liftIO $ threadDelay 1000
        
