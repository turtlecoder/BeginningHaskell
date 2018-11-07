module Chapter08.STM.AtomicTransactions.MVar where

import Control.Concurrent
import Prelude hiding (product)
import Chapter08.STM.ConcurrentResources

mainAtomicTransactionsMVar :: IO ()
mainAtomicTransactionsMVar = do v <- newMVar 10000
                                s <- newMVar [("a", 7)]
                                forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
                                forkDelay 5 $ printMoneyAndStock v s
                                _ <- getLine
                                return ()

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- takeMVar money
               let newS = map (\(k,v) -> if k == product then (k, v-1) else (k,v)) s
               putMVar money (m+price)
               putMVar stock newS
       else putMVar stock s


printMoneyAndStock :: Show a => MVar Integer -> MVar [(a, Integer)] -> IO ()
printMoneyAndStock money stock = do m <- readMVar money
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s 
