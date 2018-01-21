module Chapter02.SimpleFunctions where

import Chapter02.Data

firstOrEmpty lst = if not (null lst) then head lst else "empty"

maxmin list = if (null (tail list))
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list)))




-- Examples
p1 = Person "John" "Doe" Male

p2 = Person "Jane" "Doe" Female

p3 = Person "JJ" "Doe" Other
