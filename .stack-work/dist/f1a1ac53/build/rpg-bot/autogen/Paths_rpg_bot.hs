{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_rpg_bot (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\bin"
libdir     = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\lib\\x86_64-windows-ghc-9.4.8\\rpg-bot-1.0.0-DQoNhTZGohE9ykVF2ZRph0-rpg-bot"
dynlibdir  = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\lib\\x86_64-windows-ghc-9.4.8"
datadir    = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\share\\x86_64-windows-ghc-9.4.8\\rpg-bot-1.0.0"
libexecdir = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\libexec\\x86_64-windows-ghc-9.4.8\\rpg-bot-1.0.0"
sysconfdir = "C:\\Users\\Melissa Maureen\\Documents\\TDK\\rpg-bot\\.stack-work\\install\\746bae1e\\etc"

getBinDir     = catchIO (getEnv "rpg_bot_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "rpg_bot_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "rpg_bot_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "rpg_bot_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rpg_bot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rpg_bot_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
