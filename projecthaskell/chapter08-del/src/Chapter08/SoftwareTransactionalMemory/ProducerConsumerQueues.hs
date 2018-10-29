module Chapter08.SoftwareTransactionalMemory.ProducerConsumerQueues where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TBQueue
import GHC.Conc
import Prelude hiding (product)
import Chapter08.SoftwareTransactionalMemory.RollingBackTransactions
import System.Random

mainFn :: IO ()
mainFn = do q <- newTQueueIO
            _ <-forkIO $ backend q
            replicateM_ 10 $ forkIO $ frontend q
            _ <- getLine
            return ()

backend :: TQueue (String, Integer) -> IO ()
backend q = do m <- newTVarIO 10000
               s <- newTVarIO [("a", 7)]
               forever $ atomically $ do (product, price) <- readTQueue q
                                         pay product price m s

frontend :: TQueue (String, Integer) -> IO ()
frontend q = do (product, price) <- undefined -- get purchase information from client
                atomically $ writeTQueue q (product, price)
               

-- Excercise 8-3: Queuing Travellers
-- Implementing using TBQueue

requestTM :: TBQueue Integer -> Integer -> IO ()
requestTM tmQueue dstYear = atomically $ writeTBQueue tmQueue dstYear


travelByQTM :: TBQueue Integer -> IO ()
travelByQTM tbQueue = do delay <- atomically $ readTBQueue tbQueue
                         threadDelay (fromIntegral $ delay * 100000)

forkCustomer :: TBQueue Integer -> IO ()
forkCustomer tmq = do dstYear <- randomRIO (1999, 2018)
                      requestTM tmq dstYear
                  

mainTMLoop :: IO ()
mainTMLoop = do tmq <- newTBQueueIO 10
                replicateM_ 100 $ forkIO $ forkCustomer tmq
                forever $ travelByQTM tmq
                                       
                
