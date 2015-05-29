module Paths_webify (
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
version = Version {versionBranch = [0,1,4,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/webify-0.1.4.0/ghc-7.6.3"
datadir    = "/usr/local/share/webify-0.1.4.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "webify_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "webify_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "webify_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "webify_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
