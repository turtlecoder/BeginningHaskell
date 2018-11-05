module Chapter08.STM.ConcurrentResources where

import Control.Concurrent

updateMoney::MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   putStrLn $ "Updating Value, which is " ++ show m
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
