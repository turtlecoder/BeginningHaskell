module Chapter07.MonadsAndListsRedux.CombiningValuesUnderMonad where

import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.Writer hiding (sequence, mapM)
import Prelude hiding (sequence, mapM)

-- Example Usage for a reader
addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

-- Running a reader on a single instance
runningAddPrefix = runReader (addPrefix "World") "Hello, "

-- Usage for a reader for a list of values
addPrefixL :: [String] -> Reader String [String]
addPrefixL sl = mapM addPrefix sl


-- Running addPrefixL on a list of worlds
runningAddPrefixL = runReader (addPrefixL ["Mercury", "Venus", "Earth", "Mars"]) "Hello! "

-- Logging Information using a writer monad
logInformation :: [String] -> Writer String ()
logInformation sl = mapM_ (\s -> tell (s ++ "\n")) sl

runningLogInformation = runWriter $ logInformation ["one", "two"]

logInformationForM :: [String] -> Writer String ()
logInformationForM infos = forM_ infos $ \s -> tell (s ++ "\n")

runningLogInformationFor = runWriter $ logInformationForM ["one", "two", "three"]

-- Excercise 7-5

-- sequence implementation of sequence for lists
sequence :: (Monad m) => [m x] -> m [x]
sequence (hd:tl) = do x <- hd
                      xtl <- sequence tl
                      return $ x:xtl
sequence [] = return []

-- mapM implementation
mapM f ma = sequence.fmap f $ ma
                      

