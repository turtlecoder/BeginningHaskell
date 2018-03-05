module Prettify where

import qualified Data.Sequence as Q
import Data.Sequence hiding (length, empty, replicate, zipWith)
import Debug.Trace
import Numeric (showHex)
import Numeric (showHex)
import Data.Bits -- (shiftR, (.&.))
import Data.Char (ord)
import Prelude hiding (length)
import qualified Prelude as P(length)


punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c


text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y


hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f dl = foldr f empty dl
  

fsep :: [Doc] -> Doc
fsep docs = fold (</>) docs

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
  where
    transform []     = ""
    transform (d:ds) = case d of
                         Empty        -> transform ds
                         Char c       -> c : transform ds
                         Text s       -> s ++ transform ds
                         Line         -> '\n' : transform ds
                         a `Concat` b -> transform (a:b:ds)
                         _ `Union` b  -> transform (b:ds)


pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty         -> best col ds
            Char c        -> c : best (col + 1) ds
            Text s        -> s ++ best (col + P.length s) ds
            Line          -> '\n' : best 0 ds
            a `Concat` b  -> best col (a:b:ds)
            a `Union` b   -> nicest col (best col (a:ds))
                                        (best col (a:ds))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col 


fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w-1) `fits` cs

string :: String -> Doc
string str = (enclose '"' '"' . hcat . map oneChar) str

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r -> text r
  Nothing | mustEscape c -> hexEscape c
          | otherwise    -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

astral :: Int -> Doc
astral n = smallHex (a + 0x800) <> smallHex (b + 0xdc00)
  where a = n `shiftR` 10 .&. 0x3ff
        b = n .&. 0x3ff

hexEscape :: Char -> Doc
hexEscape c  | d < 0x10000 = smallHex d
             | otherwise = astral (d - 0x10000)
  where d = ord c

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])


smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - P.length h) 'o')
             <> text h
  where h = showHex x ""


        

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item



-- Excercise 5-1

fill :: Int -> Doc -> Doc
fill width doc = fold joinLines (linesDoc doc)
                 where
                   joinLines a b = (insertSpaces a) <> (insertSpaces b)
                   insertSpaces doc = (spaces (width - (length doc))) <> doc

linesDoc :: Doc -> [Doc]
linesDoc doc = foldr concatLines [] (toList doc)
               where concatLines :: Doc           -> [Doc]  -> [Doc]
                     concatLines l@(Line)           docs   =  l : docs
                     concatLines u@(Union _ Line)   docs   =  u : docs
                     concatLines doc                (d:ds) =  (Concat doc d) : ds
                     concatLines doc                []     =  [doc]


toList :: Doc          -> [Doc]
toList    (Concat a b) =  (toList a) ++ (toList b)
toList    doc = [doc]


length :: Doc -> Int
length (Char _) = 1
length (Text s) = P.length s
length (a `Concat` b) = (length a) + (length b)
length (a `Union`  b) = (length a) + (length b)
length _              = 0


spaces :: Int -> Doc
spaces n = Text $ replicate n ' '
