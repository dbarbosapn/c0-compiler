{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_y (
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

bindir     = "/home/enio95/.cabal/bin"
libdir     = "/home/enio95/.cabal/lib/x86_64-linux-ghc-8.6.5/y-0.1.0.0-inplace-test"
dynlibdir  = "/home/enio95/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/enio95/.cabal/share/x86_64-linux-ghc-8.6.5/y-0.1.0.0"
libexecdir = "/home/enio95/.cabal/libexec/x86_64-linux-ghc-8.6.5/y-0.1.0.0"
sysconfdir = "/home/enio95/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "y_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "y_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "y_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "y_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "y_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "y_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
