module Chapter02.SimpleFunctions where

import Chapter02.Data

firstOrEmpty lst = if not (null lst) then head lst else "empty"

maxmin list =
  if null (tail list)
    then (head list, head list)
    else ( if head list > fst (maxmin (tail list))
             then head list
             else fst (maxmin (tail list))
         , if head list < snd (maxmin (tail list))
             then head list
             else snd (maxmin (tail list)))


(+++) lst1 lst2 =
  if null lst1
    then lst2
    else head lst1 : (tail lst1 +++ lst2)

reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]
-- Examples
p1 = Person "John" "Doe" Male

p2 = Person "Jane" "Doe" Female

p3 = Person "JJ" "Doe" Other

-- Exercise 2-3 Testing functions

-- >>> maxmin [2,3,4,5]
-- (5,2)

-- >>> maxmin [1,2,1]
-- (2,1)
-- >>> maxmin []-- *** Exception: Prelude.tail: empty list

-- >>> firstOrEmpty ["Haroon", "Ahmad"]
-- "Haroon"

-- >>> reverse2 ["Haroon", "Ahmad", "Khan"]
-- ["Khan","Ahmad","Haroon"]
