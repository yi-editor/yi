{-# LANGUAGE CPP #-}

-- | System.Directory.canonicalizePath replacement
module System.CanonicalizePath
  ( canonicalizePath
  , normalisePath
  ) where

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif

import Control.Applicative
import Control.Monad
import Data.List.Split     (splitOn)
import System.FilePath     ((</>), isAbsolute, takeDirectory, pathSeparator)
import System.Posix.Files  (readSymbolicLink)


-- | Removes `/./` `//` and `/../` sequences from path,
-- doesn't follow symlinks
normalisePath :: FilePath -> FilePath
normalisePath = foldl combinePath "/" . splitPath

-- | Returns absolute name of the file, which doesn't contain
-- any `/./`, `/../`, `//` sequences or symlinks
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath =
#if !defined(mingw32_HOST_OS)
  foldM (\x y -> expandSym $ combinePath x y) "/" . splitPath
#else
  Win32.getFullPathName . normalise
#endif

-- | Dereferences symbolic links until regular
-- file/directory/something_else appears 
expandSym :: FilePath -> IO FilePath
expandSym fpath = do
  -- System.Posix.Files.getFileStatus dereferences symlink before
  -- checking its status, so it's useless here
  deref <- catch (Just <$> readSymbolicLink fpath) (\_ -> return Nothing)
  case deref of
    Just slink -> if isAbsolute slink then expandSym slink
                  else expandSym $ foldl combinePath (takeDirectory fpath) $ splitPath slink
    Nothing -> return fpath

-- | Combines two paths, moves up one level on ..
combinePath :: FilePath -> String -> FilePath
combinePath x "."  = x
combinePath x ".." = takeDirectory x
combinePath x y = x </> y

-- | Splits path into parts by path separator
splitPath :: FilePath -> [String]
splitPath = filter (not . null) . splitOn [pathSeparator]
