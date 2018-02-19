module PrettyStub where

import SimpleJSON

data Doc = ToBeDefined deriving (Show)

-- string :: String -> Doc
-- string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double n = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined


hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
