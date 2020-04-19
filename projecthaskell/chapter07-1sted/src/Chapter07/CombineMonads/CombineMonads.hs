module Chapter07.CombineMonads.CombineMonads where

import Control.Monad
import Control.Monad.Writer

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> [Writer [Int] () ]
pathsWriter' edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == end
                   subpath <- pathsWriter' edges e_end end
                   return $ do tell [start]
                               subpath
  in if start == end then tell [start] : e_paths else e_paths
    
