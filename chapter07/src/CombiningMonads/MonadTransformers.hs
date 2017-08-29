module CombiningMonads.MonadTransformers where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Common
import MonadsListsRedux.CombiningValues

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> [Writer [Int] () ]
pathsWriter' edges start end = let e_paths = do (e_start, e_end) <- edges
                                                guard $ e_start == start
                                                subpath <- pathsWriter' edges e_end end
                                                return $ do tell [start]
                                                            subpath
                               in
                                 if start == end then tell [start]  : e_paths else e_paths

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  if start == end then tell [start] `mplus` e_paths else e_paths
  where
    e_paths = do (e_start, e_end) <- lift edges
                 guard $ e_start == start
                 tell [start]
                 pathsWriterT' edges e_end end


pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end )

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift.tell $ show x
                         return $ x + 1 


runReaderWriterExample = runWriter (runReaderT readerWriterExample 3)


-- Excercise 7-6 Two States at a Time
-- Attempt 1: Passing a value to factorial
--  Example Runs
{-
runState ((execStateT $ factorial n) initialCounter=1) initialFactorial=1

-}

-- This is'nt what we want 
factorial :: Integer -> StateT Integer (State Integer) ()
factorial n = if n==0 then do return ()
              else do fac <- get
                      m <- lift get
                      put $ (m * fac)
                      lift.put $ (m+1)
                      factorial (n - 1)

-- This is the correct answer
factorial2 :: StateT Integer (State Integer) ()
factorial2 = do fac <- lift get
                n <- get
                if n==0
                  then return ()
                  else do (lift.put) (n * fac)
                          put $ (n-1)
                          factorial2
                 
-- Another variant
factorial3 :: StateT Integer (State Integer) ()
factorial3 = do fac <- get
                n <- lift get
                if n == 0
                  then return ()
                  else do put (n*fac)
                          lift.put $ (n-1)
                          factorial3
