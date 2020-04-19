{-# LANGUAGE FlexibleContexts #-}
module Chapter07.CombineMoands.MonadClasses where


import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import Chapter07.Common

readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         tell $ show x
                         return $ x + 1


readerWriterExampleMonadClasses :: (MonadReader Int m, MonadWriter String m) => m Int
readerWriterExampleMonadClasses = do x <- ask
                                     tell $ show x
                                     return $ x + 1 

-- using monad classes
usingMonadClasses = runWriter (runReaderT readerWriterExample 3)

-- using monad explicits
usingMonadsExplicitly = runRWS readerWriterExampleMonadClasses 3 0

-- Exercise 7-7
-- Write a new version of pathsWriter that holds the graph as read-only context
-- Use MonadReader for reading the graph as context
-- Use MonadWriter for writing the path
-- Use ReaderT r (WriterT w []) a and RWST r w s a
pathsWriter :: (MonadReader [(Int, Int)] m, MonadWriter [[Int]] m) => Int -> Int -> m ()
pathsWriter start end = do edges <- ask
                           let paths edges start end =
                                 let e_paths = do (e_start, e_end) <- edges
                                                  guard $ e_start == start
                                                  subpath <- paths edges e_end end
                                                  return $ start:subpath
                                 in if start==end
                                    then return [end] `mplus` e_paths
                                    else e_paths
                           let pathsFound = paths edges start end
                           tell pathsFound
                           return ()


runPathWriterRW = runWriter (runReaderT (pathsWriter 2013 2558) graph1)

runPathWriterRWS = runRWS (pathsWriter 2013 2558) graph1 ()


