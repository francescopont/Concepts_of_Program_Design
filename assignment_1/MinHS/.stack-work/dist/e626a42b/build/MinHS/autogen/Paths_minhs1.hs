{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_minhs1 (
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

bindir     = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\bin"
libdir     = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\lib\\x86_64-windows-ghc-8.6.5\\minhs1-0.1.0.0-L3tPh3RfeUF73GB0dYm6H2-MinHS"
dynlibdir  = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\share\\x86_64-windows-ghc-8.6.5\\minhs1-0.1.0.0"
libexecdir = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\libexec\\x86_64-windows-ghc-8.6.5\\minhs1-0.1.0.0"
sysconfdir = "D:\\Users\\Francesco\\Documenti\\Github\\Haskell\\concepts_of_program_design\\assignment_1\\MinHS\\.stack-work\\install\\7b47a920\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minhs1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minhs1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minhs1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minhs1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minhs1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minhs1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
