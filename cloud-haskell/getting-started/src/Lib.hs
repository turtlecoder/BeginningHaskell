module Lib
    ( localMain
    ) where

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

localMain :: IO ()
localMain = do
  Right t <- createTransport "127.0.0.1" "10501" (\_ -> ("127.0.0.1", "10501")) defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches  in order against each message  in the queue
      receiveWait [ match logMessage, match replyBack ]

      -- the say function send a message to a process registered as "logger"
      -- By default, this process simply loops through its mailbox and sends
      -- an received log message strings it finds to stderr

      say "send some messages"
    send echoPid "Hello"
    self <- getSelfPid
    send echoPid (self, "Hello")

    m <- expectTimeout 1000000
    case m of
      Nothing -> die "nothing came back"
      Just s -> say $ "got " ++ s ++ " back"
    liftIO $ threadDelay 2000000
