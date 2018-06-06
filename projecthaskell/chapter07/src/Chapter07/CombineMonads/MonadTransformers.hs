module Chapter07.CombineMonads.MonadTransformers where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
  let e_paths = do (e_start, e_end) <- lift edges
                   guard $ e_start == start
                   tell [start]
                   pathsWriterT' edges e_end end
  in
    if start == end then tell [start] `mplus` e_paths else e_paths


pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end) 


readerWriterExample :: ReaderT Int (Writer String) Int
readerWriterExample = do x <- ask
                         lift.tell $ show x
                         return $ x + 1

runReaderWriter = runWriter $ runReaderT readerWriterExample 3                     

-- Excercise 7-6
-- Calculate the factorial using 2 state monads

factorial :: StateT Integer (State Integer) ()
factorial = do fac <- lift get
               n <- get
               if n==0
                 then return ()
                 else do (lift.put) (n*fac)
                         put $ (n-1)
                         factorial

-- factorial 6
facotorial_6 = execState (execStateT factorial 6) 1 


-- Another variant for factorial
factorial2 :: StateT Integer (State Integer) ()
factorial2 = do fac <- get
                n <- lift get
                if n==0
                  then return ()
                  else do put (n*fac)
                          lift.put $ (n-1)
                          factorial2 
