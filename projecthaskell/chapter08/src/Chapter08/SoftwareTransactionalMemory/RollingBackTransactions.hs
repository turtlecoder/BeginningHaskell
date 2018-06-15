module Chapter08.SoftwareTransactionalMemory.RollingBackTransactions where

import Control.Concurrent.STM
import Chapter08.SoftwareTransactionalMemory.AtomicTransactions (updateMoneyAndStockStm)
import Prelude hiding (product)

payByCard :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
payByCard product price money stock =
  do working <- isCardSystemWorking
     if not working
       then retry
       else updateMoneyAndStockStm product price money stock


isCardSystemWorking :: STM Bool
isCardSystemWorking = undefined


pay :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
pay product price money stock = payByCard product price money stock `orElse`
                                payByCash product price money stock

payByCash :: Eq a => a -> Integer -> TVar Integer -> TVar [(a, Integer)] -> STM ()
payByCash _ _ _ _ = undefined
  
