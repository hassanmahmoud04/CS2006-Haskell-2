{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskellPractical2 (
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

bindir     = "/home/hm253/.cabal/bin"
libdir     = "/home/hm253/.cabal/lib/x86_64-linux-ghc-8.10.7/HaskellPractical2-0.1.0.0-inplace-adventure"
dynlibdir  = "/home/hm253/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/hm253/.cabal/share/x86_64-linux-ghc-8.10.7/HaskellPractical2-0.1.0.0"
libexecdir = "/home/hm253/.cabal/libexec/x86_64-linux-ghc-8.10.7/HaskellPractical2-0.1.0.0"
sysconfdir = "/home/hm253/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellPractical2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellPractical2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskellPractical2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskellPractical2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellPractical2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellPractical2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
