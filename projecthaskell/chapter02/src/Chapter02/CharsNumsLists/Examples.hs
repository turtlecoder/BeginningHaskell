module Chapter02.CharsNumsLists.Examples where

import qualified Data.Char as M(toUpper)
import Data.Ratio

a = 'a'

a_chr = M.toUpper a

frac1 = 1 % 2 + 1 % 3
frac2 = 1 % 2 - 1 % 3

ratFrac1 = toRational 1.3

toRatBack = toRational.fromRational

rev1 = reverse [1,2,3]

rev2 = reverse "abc"

lst1 = [1,2,3] ++ [4,5,6]

lst2 = "Haroon" ++ " " ++ "Khan"

data StrInt = XInt Int
            | XString String
              deriving Show

lst3 = [XInt 1, XString "Haroon", XInt 2]

lst4 = lst3 ++ [XString "Khan", XInt 3]

               
-- Excercise 2-1
lst5 = ('a':'b':'c':[]):('d':'e':[]):[]

exp1 ls = if not (null ls) then null (head ls) else False

exp2 ls = if null ls then False else if (null.tail) ls then True else False

exp3 [] = []
exp3 (hd:tl) = hd ++ exp3 tl
