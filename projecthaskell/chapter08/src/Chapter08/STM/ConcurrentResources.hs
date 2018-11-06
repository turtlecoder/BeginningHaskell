module Chapter08.STM.ConcurrentResources where

import Control.Concurrent
import System.Random
import Control.Monad
  

updateMoney::MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   tid <- myThreadId
                   putStrLn $ "Thread ID: " ++ show tid ++ " Updating Value, which is " ++ show m
                   putMVar v (m + 500)


mainUpdateMoney :: IO ()
mainUpdateMoney = do v <- newMVar 10000
                     t1 <- forkIO $ updateMoney v
                     putStrLn $ "Creating Thread 1: " ++ show t1
                     t2 <- forkIO $ updateMoney v
                     putStrLn $ "Creating Thread 2: " ++ show t2
                     t3 <- forkIO $ updateMoney v
                     putStrLn $ "Creating Thread 3: " ++ show t3
                     _ <- getLine
                     return ()


readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 tid <- myThreadId
                 putStrLn $ "Thread ID: " ++ show tid ++ " The current Value is " ++ show m

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r*1000000)


-- Create a forkdelay function, to spawn n threads with a random waiting time before
forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ do tid <- forkIO (randomDelay >> f )
                                   putStrLn $ "Creating Thread ID: " ++ show tid
                                   return ()
                 

mainRandomUpdatesReads :: IO ()
mainRandomUpdatesReads  = do v <- newMVar 10000
                             forkDelay 5 $ updateMoney v
                             forkDelay 5 $ readMoney v
                             _ <- getLine
                             return ()
