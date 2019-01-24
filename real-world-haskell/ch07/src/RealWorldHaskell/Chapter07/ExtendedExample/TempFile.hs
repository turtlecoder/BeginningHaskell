module RealWorldHaskell.Chapter07.ExtendedExample.TempFile where

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception


-- the main entry point
main :: IO ()
main = withTempFile "myTemp.txt" myAction

{- The guts of the program. Called with the path and handle of a temporary file
   . When this function exits, that file will be closed and deleted because myAction
   was called with withTempFile -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
  do -- Start by displaying a greeting on the terminal
    putStrLn $ "Welcome to tempfile.hs"
    putStrLn $ "I have a temporary file " ++ tempname
    -- lets see what the initial position is
    pos <- hTell temph
    putStrLn $ "My initial Position is " ++ (show pos)
    -- Now write some data to the temporary file
    let tempData = [1 .. 10] :: [Int]
    putStrLn $ "Writing one line containing " ++ show (length tempData) ++ " bytes: " ++ (show tempData)
    hPutStrLn temph (show tempData)
    -- Get Our new position, this does not actually modify pos in memory
    -- but makes the "pos" correspond to a different value for the reminder of the "do" block
    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos
    -- Seek to the beginning of the file and display it
    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0

    -- hGetContents performs a lazy read of the entire file
    c <- hGetContents temph

    -- Copy the file byte for byte to stdout , followed by \n
    putStrLn c

    -- Lets also display it as a Haskell literal
    putStrLn $ "Which could be expressed as this Haskell literal: "
    print c


 {- This function takes two parameters: a file name pattern and another function
    It will create a temporary file , and pass the name and handle of that file to the
    given function.

The temporary file is created with openTempFile. The directory is the one
indiciated by getTemporaryDirectoryif the system has no notion of
a temporary directory, "." is used. The given pattern is passed to openTempFile.
After the given function terminates, even if it terminates due to an
exception, the Handle is closed and the file is deleted. -}

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
  do tempDir <- (catch (getTemporaryDirectory) ((\e -> return ".") :: (IOException -> IO String)))
     (tempFile, temph) <- openTempFile tempDir pattern
     finally (func tempFile temph)
             (do hClose temph
                 removeFile tempFile)


