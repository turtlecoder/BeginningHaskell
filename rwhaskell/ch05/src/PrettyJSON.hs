module PrettyJSON( renderJValue
                 , kvalue
                 , zvalue
                 ) where

import SimpleJSON -- (JValue(..))
import Numeric (showHex)
import Data.Bits -- (shiftR, (.&.))
import Data.Char (ord)
import Prettify 

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "True"
renderJValue (JBool False) = text "False"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary)  = series ((char '[') `Concat` softline) (char ']') renderJValue ary
renderJValue (JObject obj) = series ((char '{') `Concat` softline) (char '}') field obj
  where field (name, val)  = string name <> text ": " <> renderJValue val


kvalue = JObject [("f", JNumber 1), ("q", JBool True)]
zvalue = JObject [("f", JArray [JNumber 1, JNumber 2, JNumber 3])]
arr = JArray [JNumber 1, JNull, JNumber 2, JBool True, JBool False, kvalue, zvalue]
