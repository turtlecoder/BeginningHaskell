module SearchProblems.LogicMonad where

import Control.Monad.Logic
import Common


pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end =
  let e_paths = do (e_start, e_end) <- choices edges
                   guard $ e_start == start
                   subpath <- pathsL edges e_end end
                   return $ start:subpath
  in if start == end then return [end] `mplus` e_paths else e_paths


choices :: [a] -> Logic a
choices = msum . map return

observeGraph2 = observeMany 3 $ pathsL graph2 2013 2558

-- Excercise 7-4: Not Tested , but should work corrctly 

pathsL2 :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL2 edges start end =
  let e_paths = choices edges >>-
               \(e_start, e_end) -> guard (e_start==e_end) >>
               pathsL2 edges e_end end >>-
               \ subpath -> (return $ start:subpath)
  in if start == end then return [end] `mplus` e_paths else e_paths

observeGraph3 = observeMany 3 $ pathsL2 graph2 2013 2558               
    
