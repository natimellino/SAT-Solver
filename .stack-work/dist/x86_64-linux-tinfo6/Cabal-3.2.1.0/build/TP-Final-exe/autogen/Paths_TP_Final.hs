{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP_Final (
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

bindir     = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/bin"
libdir     = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/lib/x86_64-linux-ghc-8.10.3/TP-Final-0.1.0.0-1DFzIMjMfjN8tIgw3SUWfI-TP-Final-exe"
dynlibdir  = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/lib/x86_64-linux-ghc-8.10.3"
datadir    = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/share/x86_64-linux-ghc-8.10.3/TP-Final-0.1.0.0"
libexecdir = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/libexec/x86_64-linux-ghc-8.10.3/TP-Final-0.1.0.0"
sysconfdir = "/home/nati/FCEIA/SAT-Solver/.stack-work/install/x86_64-linux-tinfo6/6922c64f7c78fd132ca5a8eeeb4368b91ad3501105b3486e48ed41e21da0895e/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP_Final_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP_Final_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP_Final_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP_Final_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP_Final_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP_Final_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
