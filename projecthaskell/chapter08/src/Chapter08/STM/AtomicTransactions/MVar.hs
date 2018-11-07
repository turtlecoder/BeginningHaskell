module Chapter08.STM.AtomicTransactions.MVar where

import Control.Concurrent

mainAtomicTransactionsMVar :: IO ()
mainAtomicTransactionsMVar = undefined

updateMoneyAndStock :: Eq a => a -> Integer -> MVar Integer -> MVar [(a, Integer)] -> IO ()
updateMoneyAndStock _ _ _ _ = undefined
