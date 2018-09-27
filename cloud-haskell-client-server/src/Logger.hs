{-# LANGUAGE RecordWildCards #-}

module Logger where

import Control.Distributed.Process.Node (runProcess, forkProcess, LocalNode)
import Control.Distributed.Process (liftIO, match, receiveWait, register, Process, nsend)
import Types

runChatLogger :: LocalNode -> IO ()
runChatLogger node = do
  putStrLn "runChatLogger"
  logger <- forkProcess node chatLogger
  putStrLn $ "Forked Process Chat Logger: " ++ (show logger)
  runProcess node (register "chatLogger" logger)
  putStr "Running ChatLogger"

logStr :: String -> Process ()
logStr = nsend "chatlogger"

chatLogger :: Process ()
chatLogger = receiveWait [ match $ \chatMessage -> do
                             liftIO . putStrLn $ chatMessageToStr chatMessage
                             chatLogger
                         , match $ \str -> do
                             liftIO . putStrLn $ str
                             chatLogger
                         ]

chatMessageToStr :: ChatMessage -> String
chatMessageToStr ChatMessage{..} = case from of
                                     Server -> message
                                     Client sender -> sender ++ ": " ++ message


logChatMessage :: ChatMessage -> Process ()
logChatMessage = nsend "chatLogger"
