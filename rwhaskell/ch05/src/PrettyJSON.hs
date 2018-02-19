module PrettyJSON where

import SimpleJSON
import PrettyStub

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "True"
renderJValue (JBool False) = text "False"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str


string :: String -> Doc
string str = (enclose '"' '"' . hcat . map oneChar) str


