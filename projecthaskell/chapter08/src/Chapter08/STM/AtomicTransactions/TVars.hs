module Chapter08.STM.AtomicTransactions.TVars where

import Control.Concurrent.STM.TVar
import Control.Concurrent.Async
import Control.Monad.STM
import Prelude hiding (product)
import Chapter08.STM.ConcurrentResources


updateMoneyAndStock::Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
updateMoneyAndStock product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k==product then (k, v-1) else (k,v)) s
               writeTVar money (m+price)
               writeTVar stock newS
       else return ()


mainAtomicTransactionsTVar :: IO ()
mainAtomicTransactionsTVar = do
  putStrLn "\nmainAtomicTransactionsTVar\n=========================="
  v <- newTVarIO 10000
  s <- newTVarIO [("a", 7)]
  tidList <- forkDelay 10 $ atomically $ updateMoneyAndStock "a" 1000 v s
  mapM_ (\tid -> wait tid) tidList
  putStrLn "After thread delay"
  return ()
