module MutableRefsST where

listLength :: [a] -> Integer
listLength list = runST $ do l <- newSTRef 0
			       traverseList list l
			       readSTRef l
			       where traverseList []
			       
