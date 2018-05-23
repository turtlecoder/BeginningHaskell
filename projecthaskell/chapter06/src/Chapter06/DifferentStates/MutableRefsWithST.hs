{-# LANGUAGE RankNTypes #-}

module Chapter06.DifferentStates.MutableRefsWithST where

import Control.Monad.ST
import Data.STRef
import Chapter06.KMeansCommon

listLength :: [a] -> Integer
listLength alist = runST $ do l <- newSTRef 0
                              traverseList alist l
                              readSTRef l
                                where traverseList []     _ = return ()
                                      traverseList (_:xs) l = do modifySTRef' l (+1)
                                                                 traverseList xs l

