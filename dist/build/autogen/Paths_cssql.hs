{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cssql (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/amc/.cabal/bin"
libdir     = "/Users/amc/.cabal/lib/x86_64-osx-ghc-8.6.5/cssql-0.1.0.0-361dYGKcZQ1HtFQjx6Wjy"
dynlibdir  = "/Users/amc/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/amc/.cabal/share/x86_64-osx-ghc-8.6.5/cssql-0.1.0.0"
libexecdir = "/Users/amc/.cabal/libexec/x86_64-osx-ghc-8.6.5/cssql-0.1.0.0"
sysconfdir = "/Users/amc/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cssql_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cssql_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cssql_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cssql_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cssql_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cssql_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
