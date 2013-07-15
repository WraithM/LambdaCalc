module Paths_fpcompletetest (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/matt/.cabal/bin"
libdir     = "/home/matt/.cabal/lib/fpcompletetest-0.1.0.0/ghc-7.6.3"
datadir    = "/home/matt/.cabal/share/fpcompletetest-0.1.0.0"
libexecdir = "/home/matt/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "fpcompletetest_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fpcompletetest_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fpcompletetest_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fpcompletetest_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
