--{-# LANGUAGE NoImplicitPrelude #-}

module MonadsListsRedux.CombiningValues where

-- import Control.Monad.Reader (Reader, ask, return, (>>=), mapM, mapM_)
-- import Control.Monad.Writer (Writer, Monad, tell, runWriter, forM_)
-- import Prelude (String, ($), (++))

import Control.Monad.Reader hiding (sequence, mapM)
import Control.Monad.Writer hiding (sequence, mapM)
import Prelude hiding (sequence, mapM)


addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s 


--

addPrefixL :: [String] -> Reader String [String]
addPrefixL = mapM addPrefix

--
logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))
                       
logInfoRun = runWriter $ logInformation ["one", "two"]


--
forLogInformation :: [String] -> Writer String ()
forLogInformation infos = forM_ infos $ \s -> tell (s++"\n")


runForLogInfo = runWriter $ forLogInformation ["one", "two"]

-- Excercise 7-5

sequence :: Monad m => [ m a ] -> m [a]
sequence [] = return []
sequence (mx:mxs) = do x <- mx
                       xs <- sequence mxs
                       return $ x:xs

mapM f ma = sequence.fmap f $ ma


--

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f*x)) 1 [ 1 .. n ]

f6 = factorialSteps 6 

--

powerSet :: [a] -> [[a]]
powerSet xl = filterM (\_ -> [False, True]) xl
