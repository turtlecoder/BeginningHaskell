module Paths_chapter02 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/hkhanhex/.cabal/bin"
libdir     = "/Users/hkhanhex/.cabal/lib/x86_64-osx-ghc-7.10.3/chapter02-0.0.1-DBYWzBPAj2v1czNkXNBY9l"
datadir    = "/Users/hkhanhex/.cabal/share/x86_64-osx-ghc-7.10.3/chapter02-0.0.1"
libexecdir = "/Users/hkhanhex/.cabal/libexec"
sysconfdir = "/Users/hkhanhex/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chapter02_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chapter02_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "chapter02_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chapter02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chapter02_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
