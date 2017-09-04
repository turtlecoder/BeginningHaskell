{-# LANGUAGE FlexibleContexts #-}
module CombiningMonads.MonadClasses where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS.Lazy
import Control.Applicative

--
import Common

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         tell $ show x
                         return $ x + 1 

readerWriterExampleMC :: (MonadReader Int m , MonadWriter String m) => m Int
readerWriterExampleMC = do x <- ask
                           tell $ show x
                           return $ x+1

runRWMC1 = runWriter (runReaderT readerWriterExampleMC 3)

runRWMC2 = runRWS readerWriterExampleMC 3 0 

-- Excercise 7-6
{-
Write a new version of pathsWriter that holds the graph as a read-only context. This means you need to use functionality from both MonadReader (for handling the graph) and MonadWriter (for handling the paths) wrapping the base list monad. To check that the function is general, use two different monads to provide the requested functionality: ReaderT r (WriterT w []) a and RWST r w s a.
-}

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start:subpath
  in if start==end
     then return [end] `mplus` e_paths
     else e_paths

{- This is not really a good solution, but just a first attempt -}
pathsWriter1 :: (MonadReader [(Int, Int)] m, MonadWriter [[Int]] m) => Int -> Int -> m ()
pathsWriter1 start end = do edges <- ask
                            let pathsFound = paths edges start end
                            tell pathsFound
                            return ()
{- This is my 2nd attempt, but I'm ending in an infinite loop
   Going to move on for now and try it later. 
 -}
pathsWriter2 :: (MonadReader [(Int, Int)] m, MonadWriter [[Int]] m ) => Int -> Int -> m ()
pathsWriter2 start end = do edges <- ask
                            let
                              pathsFound :: Int -> Int -> WriterT [Int] [] ()
                              pathsFound n_start n_end =
                                let e_paths = do (e_start, e_end) <- lift edges
                                                 guard $ e_start==n_start
                                                 tell [n_start]
                                                 pathsFound e_start n_end
                                in
                                  if n_start==n_end then tell [n_start] `mplus` e_paths else e_paths
                              in
                              tell (execWriterT $ pathsFound start end)

runPathWriterRW = runWriter (runReaderT (pathsWriter1 2013 2558) graph1)

-- This is still not working, 
runPathWriter2Rw = runWriter (runReaderT (pathsWriter2 2013 2558) graph1)


