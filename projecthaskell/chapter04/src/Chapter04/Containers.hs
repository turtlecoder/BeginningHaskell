module Chapter04.Containers where

import qualified Data.Map as M


anEmptyMap = M.empty

aSingleton = M.singleton "hello" 3

anExampleList = M.fromList [("hello", 1), ("bye", 2), ("hello", 3)]

mlist = let m1 = M.singleton "hello" 3
            m2 = M.insert "bye" 2 m1
            m3 = M.insert "hello" 5 m2
            m4 = M.insertWith (+) "hello" 7 m3
        in (m1, m2, m3, m4)

isEmptyTrue = M.null M.empty

isEmptyFalse = M.null $ M.fromList [("hello", 3), ("bye", 4)]

isMemberTrue = M.member "hello" $ M.fromList[("hello", 3), ("bye", 4)]

lookupHello = M.lookup "hello" $ M.fromList[("hello", 3), ("bye", 4)]
