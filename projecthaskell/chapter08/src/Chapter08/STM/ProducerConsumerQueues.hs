module Chapter08.STM.ProducerConsumerQueues where

import Control.Concurrent.STM.TBQueue
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import GHC.Conc
import Debug.Trace
import System.Random

mainProducerConsumer :: IO ()
mainProducerConsumer = do
  traceIO "\nRunning TimeMachine Store on Producer Consumers\n===================="
  tmq <- newTBQueueIO 10
  -- create time machine customers
  threadIdLst <- replicateM 4 $ async $ forkCustomer tmq
  processTimeMachineQueue tmq 4
  traceIO "Begin Waiting on customer threads..."
  mapM_ (\tid -> wait tid) threadIdLst
  traceIO "Done Waiting for all threads"

forkCustomer :: TBQueue Integer -> IO ()
forkCustomer tmq = do dstYear <- randomRIO (2010, 2018) :: IO Integer                      
                      atomically $ requestTimeMachine tmq dstYear
                      traceIO $ "Customer Travelling to: " ++ (show dstYear)
                      
                      
requestTimeMachine :: TBQueue Integer -> Integer -> STM ()
requestTimeMachine tmQueue year = writeTBQueue tmQueue year

processTimeMachineQueue :: TBQueue Integer -> Integer -> IO ()
processTimeMachineQueue tmq counter =
  do traceIO $ "Processing Customer Thread " ++ (show counter)
     dstYear <- atomically $ readTBQueue tmq
     traceIO $ "Travelling to " ++ show dstYear
     randomTravelTime <- randomRIO (1,2019 - dstYear) :: IO Integer
     let travelTime = randomTravelTime * 1000000        
     threadDelay $ fromIntegral travelTime
     traceIO $ "Reached year " ++ show dstYear ++ " in " ++
       show ((fromIntegral travelTime / 1000000)::Double) ++ " seconds"
     let nextJob = counter - 1
     if nextJob==0
       then return ()
       else processTimeMachineQueue tmq nextJob
      
      
