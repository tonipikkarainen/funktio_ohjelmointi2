{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ties341 (
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

bindir     = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/bin"
libdir     = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/lib/x86_64-osx-ghc-8.6.5/ties341-0.1.0.0-5IQHWxm0wqPLjv2D0j6utw-ties341"
dynlibdir  = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/share/x86_64-osx-ghc-8.6.5/ties341-0.1.0.0"
libexecdir = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/libexec/x86_64-osx-ghc-8.6.5/ties341-0.1.0.0"
sysconfdir = "/Users/tonipikkarainen/master_degree/funktio2/ties341/.stack-work/install/x86_64-osx/5f70fba4a9b7feedb103cc5bbea04c9a958feaca1415e27a352726ed8d0fe43c/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ties341_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ties341_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ties341_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ties341_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ties341_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ties341_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
