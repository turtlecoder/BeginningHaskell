module Chapter07.SearchProblems.GraphPaths where

import Control.Monad
import Control.Monad.Logic

-- incorrect Implementation
-- paths::[(Int, Int)] -> Int -> Int -> [[Int]]
-- paths edges start end = do (e_start, e_end) <- edges
--                            guard $ e_start == e_end
--                            subpath <- paths edges e_end end
--                            return $ start:subpath

paths::[(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start:subpath
  in if start == end
     then return [end] `mplus` e_paths
     else e_paths


graph1::[(Int, Int)]
graph1 = [(2013, 501), (2013,1004), (501, 2558), (1004, 2558)]

graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 2558)]

pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end = let e_paths = do (e_start, e_end) <- choices edges
                                          guard $ e_start == start
                                          subpath <- pathsL edges e_end end
                                          return $ start:subpath
                         in if start == end then return [end] `mplus` e_paths else e_paths
                           

choices :: [a] -> Logic a
choices = msum.map return

observeMany3 = observeMany 3 $ pathsL graph1 2013 2558


showMplus = [1,2] `mplus` [3,4]

interleavelists = [1,2] `interleave` [3,4]

-- Excercise 7-4: Desugar Monad Notation
desugaredPathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
desugaredPathsL edges start end =
  let e_paths = choices edges >>=
                (\(e_start, e_end) -> (guard $ e_start == e_end) >>
                  ((pathsL edges e_end end) >>=
                    (\subpath -> return $ start:subpath)))
  in if start == end then return [end] `mplus` e_paths else e_paths


-- Somethign is wrong here, probably copied the code incorrectly
pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
  let e_paths = choices edges >>-
                \(e_start, e_end) -> guard (e_start == e_end) >>
                pathsLFair edges e_end end >>-
                \subpath -> return $ start:subpath
  in if start == end
     then return [end] `interleave` e_paths else e_paths
    
observeManyPathsLFair = observeMany 3 $ pathsLFair graph2 2013 2558

