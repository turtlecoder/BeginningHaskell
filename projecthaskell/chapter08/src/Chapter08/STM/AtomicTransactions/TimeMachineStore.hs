module Chapter08.STM.AtomicTransactions.TimeMachineStore where

import qualified Data.Set as S
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent
import System.Random
import Chapter08.STM.ConcurrentResources
import Debug.Trace

-- The set of Time Machines used, Initially empty
type TimeMachinesUsed = S.Set Integer
type AvailableTimeMachines = Integer
type TargetYear = Integer


-- Exercise 8-2
-- ------------
timeMachineSimulation :: TVar TimeMachinesUsed -> AvailableTimeMachines -> TargetYear -> STM ()
timeMachineSimulation currentTMsInUse ul targetYear =
  do tmsInUse <- readTVar currentTMsInUse
     if ((toInteger $ S.size tmsInUse) >= ul || targetYear `S.member` tmsInUse)
       then trace "Retrying..." retry
       else trace "Updating Time Machine" $ writeTVar currentTMsInUse $ S.insert targetYear tmsInUse

updateTimeMachineStore :: TVar TimeMachinesUsed -> TargetYear -> STM ()
updateTimeMachineStore currentTMsInUse targetYear = do
  tmsInUse <- readTVar currentTMsInUse
  writeTVar currentTMsInUse $ S.delete targetYear tmsInUse

timeMachinesSimulationIO :: IO ()
timeMachinesSimulationIO = do
  tmStore <- (newTVarIO S.empty)
  tidList <- forkDelay 5 $ do
    targetYear <- randomRIO (2011, 2015) :: IO Integer -- get random year.
    trace ("Target Year: " ++ (show targetYear)) $ (atomically $ timeMachineSimulation tmStore 2 targetYear)
    targetTime <- randomRIO (1000000, 5000000):: IO Int
    putStrLn $ "Travelling to " ++ (show targetYear) ++ " in " ++ (show ((fromIntegral targetTime)/1000000)) ++ "s"
    _ <- threadDelay targetTime
    atomically $ updateTimeMachineStore tmStore targetYear
    return ()
  mapM_ (\tid -> wait tid) tidList
  return ()


