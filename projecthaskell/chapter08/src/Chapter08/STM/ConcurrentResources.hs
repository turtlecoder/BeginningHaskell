module Chapter08.STM.ConcurrentResources where

import Control.Concurrent
import Control.Concurrent.Async
import System.Random
import Control.Monad
  

updateMoney::MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   tid <- myThreadId
                   putStrLn $ "Thread ID: " ++ show tid ++ " Updating Value, which is " ++ show m
                   putMVar v (m + 500)


mainUpdateMoney :: IO ()
mainUpdateMoney = do putStrLn "\nmainUpdateMoney\n================="
                     v <- newMVar 10000
                     t1 <- async $ updateMoney v
                     putStrLn $ "Creating Thread 1: " ++ (show $ asyncThreadId t1)
                     t2 <- async $ updateMoney v
                     putStrLn $ "Creating Thread 2: " ++ (show $ asyncThreadId t2)
                     t3 <- async $ updateMoney v
                     putStrLn $ "Creating Thread 3: " ++ (show $ asyncThreadId t3)
                     wait t1
                     wait t2
                     wait t3
                     return ()


readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 tid <- myThreadId
                 putStrLn $ "Thread ID: " ++ show tid ++ " The current Value is " ++ show m

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r*1000000)


-- Create a forkdelay function, to spawn n threads with a random waiting time before

forkDelay :: Int -> IO a -> IO [Async a]
forkDelay n f = replicateM n $ do tid <- async (randomDelay >> f )
                                  putStrLn $ "Creating Thread ID: " ++ (show $ asyncThreadId tid)
                                  return tid
                 

mainRandomUpdatesReads :: IO ()
mainRandomUpdatesReads  = do putStrLn "\nmainRandomUpdatesRead\n======================"
                             v <- newMVar 10000
                             tidList1 <- forkDelay 5 $ updateMoney v
                             tidList2 <- forkDelay 5 $ readMoney v
                             mapM_ (\tid -> wait tid) (tidList1 ++ tidList2) 
                             return ()
