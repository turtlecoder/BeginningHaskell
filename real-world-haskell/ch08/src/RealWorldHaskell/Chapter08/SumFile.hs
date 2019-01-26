module RealWorldHaskell.Chapter08.SumFile where

main = do contents <- getContents
          print (sumFile contents)
            where sumFile = sum.fmap read . words
