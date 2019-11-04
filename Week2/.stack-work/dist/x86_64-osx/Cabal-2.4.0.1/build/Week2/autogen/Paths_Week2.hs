{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Week2 (
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

bindir     = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/bin"
libdir     = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/lib/x86_64-osx-ghc-8.6.5/Week2-0.1.0.0-Ghi4v74EYOyUdsvCkhV12-Week2"
dynlibdir  = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/share/x86_64-osx-ghc-8.6.5/Week2-0.1.0.0"
libexecdir = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/libexec/x86_64-osx-ghc-8.6.5/Week2-0.1.0.0"
sysconfdir = "/Users/tonipikkarainen/master_degree/funktio2/Week2/.stack-work/install/x86_64-osx/c4e8a4d2753acf12ea65fed6e7f28e7d312495df0332bc292e155ce8aa3cb63a/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Week2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Week2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Week2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Week2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Week2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Week2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
