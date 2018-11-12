module Chapter08.STM.AtomicTransactions.TimeMachineStore where

import qualified Data.Set as S
import Control.Concurrent.STM
import System.Random
import Chapter08.STM.ConcurrentResources

-- The set of Time Machines used, Initially empty
type TimeMachinesUsed = S.Set Integer
type AvailableTimeMachines = Integer
type TargetYear = Integer

timeMachineSimulation :: TVar TimeMachinesUsed -> AvailableTimeMachines -> TargetYear -> STM ()
timeMachineSimulation currentTMsInUse ul targetYear =
  do tmsInUse <- readTVar currentTMsInUse
     if ((toInteger $ S.size tmsInUse) >= ul || targetYear `S.member` tmsInUse)
       then retry
       else writeTVar currentTMsInUse $ S.insert targetYear tmsInUse

timeMachinesSimulationIO :: IO ()
timeMachinesSimulationIO = do
  tmStore <- (newTVarIO S.empty)
  forkDelay 5 $ do
    targetYear <- randomRIO (2011, 2015) -- get random year. 
    atomically $ timeMachineSimulation tmStore 2 targetYear
  _ <- getLine
  return ()


