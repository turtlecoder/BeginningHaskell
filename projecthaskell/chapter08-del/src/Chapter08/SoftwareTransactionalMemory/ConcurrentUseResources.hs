module Chapter08.SoftwareTransactionalMemory.ConcurrentUseResources where

import Control.Concurrent
import System.Random
import Control.Monad


mainFn :: IO ()
mainFn = do v <- newMVar 10000
            _ <- forkIO $ updateMoney v
            _ <- forkIO $ updateMoney v
            _ <- forkIO $ updateMoney v
            _ <- getLine
            return ()


updateMoney :: MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v
                   putStrLn $ "Updating value, which is " ++ show m
                   putMVar v (m+500) -- suppose a constant price

readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 putStrLn $ "The current value is " ++ show m 

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3,15)
                 threadDelay ( r * 100000)


forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

mainFn2 :: IO ()
mainFn2 = do v <- newMVar 10000
             forkDelay 5 $ updateMoney v
             forkDelay 5 $ readMoney v
             _ <- getLine
             return ()
             
