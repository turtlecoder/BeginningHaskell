{-# LANGUAGE RecordWildCards #-}

module Logger where

import Control.Distributed.Process.Node (runProcess, forkProcess, LocalNode)
import Control.Distributed.Process (liftIO, match, receiveWait, register, Process, nsend, say)
import Types

runChatLogger :: LocalNode -> IO ()
runChatLogger node = do
  putStrLn "runChatLogger"
  logger <- forkProcess node chatLogger
  putStrLn $ "Forked Process Chat Logger: " ++ (show logger)
  runProcess node (register "chatLogger" logger)
  putStrLn "Running ChatLogger"

logStr :: String -> Process ()
logStr = nsend "chatLogger"

chatLogger :: Process ()
chatLogger = do
  (liftIO.putStrLn) "Waiting for Log Messages"
  receiveWait [ match $ \chatMessage -> do
                  say $ "Got a Chat Message"
                  liftIO . putStrLn $ chatMessageToStr chatMessage
                  chatLogger
              , match $ \str -> do
                  say $ "Got Message: " ++ str
                  (liftIO.putStrLn) str
                  chatLogger
              ]

chatMessageToStr :: ChatMessage -> String
chatMessageToStr ChatMessage{..} = case from of
                                     Server -> message
                                     Client sender -> sender ++ ": " ++ message


logChatMessage :: ChatMessage -> Process ()
logChatMessage = nsend "chatLogger"
