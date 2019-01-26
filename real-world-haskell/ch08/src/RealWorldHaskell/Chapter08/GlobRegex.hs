module RealWorldHaskell.Chapter08.GlobRegex where

import Text.Regex.Posix ((=~))
import Data.Char

globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' ""  = ""
globToRegex' ('*' : cs) = ".*" ++ globToRegex' cs
globToRegex' ('?' : cs) = "." ++ globToRegex' cs
globToRegex' ('[':'!': c :cs ) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error  "Unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat


-- Exercise 8-1
-- *RealWorldHaskell.Chapter08.GlobRegex Text.Regex.Posix Data.ByteString.Char8> globToRegex "[abcde"
-- "^[abcde*** Exception: Unterminated character class
-- CallStack (from HasCallStack):
--   error, called at src/RealWorldHaskell/Chapter08/GlobRegex.hs:23:22 in main:RealWorldHaskell.Chapter08.GlobRegex

-- Exercise 8-2
imatchesGlob :: Bool -> FilePath -> String -> Bool
imatchesGlob ignoreCase name pat =
  if ignoreCase
  then (fmap toLower name) `matchesGlob` (fmap toLower pat)
  else name `matchesGlob` pat
