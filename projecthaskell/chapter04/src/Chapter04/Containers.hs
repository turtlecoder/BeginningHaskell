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

lookupWelcome = M.lookup "welcome" $ M.fromList [("hello", 3) , ("bye", 4)]

lookupWithDefault = M.findWithDefault 0 "welcome" $ M.fromList [("hello", 3), ("bye", 4)]

deleteHello = M.delete "hello" $ M.fromList [("hello", 3), ("bye", 4)]

adjustMap = M.adjust (+7) "hello" $ M.fromList [("hello", 3), ("bye", 4)]

alterMap = M.alter (\(Just v) -> Just (v+7)) "hello" $ M.fromList [("hello", 3), ("bye", 4)]

-- Excercise 4-2

delete :: Ord k => k -> (M.Map k v) -> (M.Map k v)
delete key map = M.alter (\(Just v) -> Nothing) key map

insert :: Ord k => k -> v -> (M.Map k v) -> (M.Map k v)
insert key value map = M.alter (\ _ -> Just value) key map

adjust :: Ord k => (v -> v) -> k -> M.Map k v -> M.Map k v
adjust f key map = M.alter (\(Just v) -> Just (f v)) key map

---

unionIntersection = let m1 = M.fromList [("hello", 3), ("bye", 4)]
                        m2 = M.fromList [("hello", 5), ("welcome", 6)]
                    in (m1 `M.union` m2, M.intersectionWith (-) m1 m2)
                                            
