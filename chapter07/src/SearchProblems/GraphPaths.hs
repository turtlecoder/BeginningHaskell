module SearchProblems.GraphPaths where

import Control.Monad

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start:subpath
  in if start == end
     then return [end] `mplus` e_paths
     else e_paths

graph1 :: [(Int, Int)]
graph1 = [(2013, 501) , (2013, 1004), (501, 2558), (1004, 2558)]

paths2 :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths2 edges start end = let e_paths = do (e_start, e_end) <- edges
                                          guard $ e_start == start
                                          subpath <- paths2 edges e_end end
                                          return $ start:subpath
                         in
                           if start == end
                           then return [end] `mplus` e_paths
                           else e_paths

                           
                                          
