{-# LANGUAGE CPP #-}

-- | System.Directory.canonicalizePath replacement
module System.CanonicalizePath
  ( canonicalizePath
  ) where

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif

import Control.Applicative
import Control.Monad
import Data.List.Split     (splitOn)
import System.FilePath     ((</>), isAbsolute, takeDirectory)
import System.Posix.Files  (readSymbolicLink)

-- Returns absolute name of the file, which doesn't contain
-- any '.', '..' or symlinks
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath fpath =
#if !defined(mingw32_HOST_OS)
  foldM (\x y -> expandSym $ combinePath x y) "/" $ splitOn "/" fpath
#else
  Win32.getFullPathName $ normalise fpath
#endif

-- | Dereference symbolic links until regular
-- file/directory/something_else appears 
expandSym :: FilePath -> IO FilePath
expandSym fpath = do
  -- System.Posix.Files.getFileStatus dereferences symlink before
  -- checking its status, so it's useless
  deref <- catch (Just <$> readSymbolicLink fpath) (\_ -> return Nothing)
  case deref of
    Just slink -> if isAbsolute slink then expandSym slink
                  else expandSym $ foldl combinePath (takeDirectory fpath) $ splitOn "/" slink
    Nothing -> return fpath

-- | Combine two paths, move up one level on ..
combinePath :: FilePath -> String -> FilePath
combinePath x "."  = x
combinePath x ".." = takeDirectory x
combinePath x y = x </> y
