module RealWorldHaskell.Chapter08.Glob(namesMatching, isPattern) where

import System.Directory( doesDirectoryExist
                       , doesFileExist
                       , getCurrentDirectory
                       , getDirectoryContents)
import System.FilePath ( dropTrailingPathSeparator
                       , splitFileName
                       , (</>)
                       , pathSeparator)
import Control.Exception (handle)
import Control.Monad (forM)
import RealWorldHaskell.Chapter08.GlobRegex (matchesGlob, imatchesGlob)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
              then getCurrentDirectory
              else return dirName
  handle ((const (return [])) :: (IOError -> IO [String])) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                 then filter isHidden names
                 else filter (not.isHidden ) names
    -- return (filter (`matchesGlob` pat) names)
    return (filter (\name -> imatchesGlob ignoreCase name pat) names)


isHidden ('.':_) = True
isHidden _       = False
    
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
                 

namesMatching pat | not (isPattern pat) = do
                      exists <- doesNameExist pat
                      return (if exists then [pat] else [])
                  | otherwise = do
                      case splitFileName pat of
                        ("", baseName) -> do
                          curDir <- getCurrentDirectory
                          listMatches curDir baseName
                        (dirName, baseName) ->
                          do dirs <- if isPattern dirName
                                     then namesMatching (dropTrailingPathSeparator dirName)
                                     else return [dirName]
                             let listDir = if isPattern baseName
                                           then listMatches
                                           else listPlain
                             pathNames <- forM dirs $ \dir -> do
                               baseNames <- listDir dir baseName
                               return (map (dir </>) baseNames)
                             return (concat pathNames)

-- Exercises On Making Use of our Pattern Matchers
--------------------------------------------------
-- 1 
                      
-- TODO maybe test this on windows
ignoreCase = case pathSeparator of
  '/' -> False    -- Posix 
  '\\' -> True    -- Windows


