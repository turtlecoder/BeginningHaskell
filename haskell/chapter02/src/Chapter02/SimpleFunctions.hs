module Chapter02.SimpleFunctions where

firstOrEmpty lst = if not (null lst) then head lst else "empty"

maxmin list = if (null (tail list))
              then (head list, head list)
              else ( if (head list) > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if (head list) < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list)))



data Client = GovOrg String
            | Company String Integer String String
            | Individual Person Bool
            deriving Show


data Person = Person String String Gender
              deriving Show

-- Ex 2-4

data Gender = Male | Female | Other deriving Show

-- Examples
p1 = Person "John" "Doe" Male

p2 = Person "Jane" "Doe" Female

p3 = Person "JJ" "Doe" Other
