module Chapter08.SoftwareTransactionalMemory.AtomicTransactions where

import Control.Concurrent.MVar
import Chapter08.SoftwareTransactionalMemory.ConcurrentUseResources
import Prelude hiding (product)
import Control.Concurrent.STM

mainFn :: IO ()
mainFn = do v <- newMVar 10000
            s <- newMVar [("a",7)]
            forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
            forkDelay 5 $ printMoneyAndStock v s
            _ <- getLine
            return ()

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a,Integer)] -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- takeMVar money
               let newS = map (\(k,v) -> if k == product
                                         then (k,v-1)
                                         else (k,v)) s
               putMVar money (m+price) >> putMVar stock newS
     else putMVar stock s 

printMoneyAndStock :: Show a => MVar Integer -> MVar [(a,Integer)] -> IO ()
printMoneyAndStock money stock = do m <- readMVar money
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s

updateMoneyAndStockStm :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStockStm product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k == product then (k,v-1) else (k,v)) s
               writeTVar money (m+price) >> writeTVar stock newS
       else return ()


mainStm :: IO ()
mainStm = do v <- newTVarIO 10000
             s <- newTVarIO [("a", 7)]
             forkDelay 5 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
             _ <- getLine
             return ()
