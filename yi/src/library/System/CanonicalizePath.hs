{-# LANGUAGE CPP #-}

-- | System.Directory.canonicalizePath replacement
module System.CanonicalizePath
  ( canonicalizePath
  , normalisePath
  , replaceShorthands
  ) where

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
import System.FilePath (normalise)
#endif

import Control.Applicative
import Control.Monad
import Data.List.Split     (splitOn, splitOneOf)
import System.FilePath     ((</>), isDrive, isAbsolute, takeDirectory, pathSeparator, pathSeparators)
import System.Directory    (getCurrentDirectory)
import System.PosixCompat.Files  (readSymbolicLink)
import Control.Exc          (ignoringException)


-- | Removes `/./` `//` and `/../` sequences from path,
-- doesn't follow symlinks
normalisePath :: FilePath -> IO FilePath
normalisePath path = do
  absPath <- makeAbsolute path
  return $ foldl combinePath "/" $ splitPath absPath

-- | Returns absolute name of the file, which doesn't contain
-- any `/./`, `/../`, `//` sequences or symlinks
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
#if !defined(mingw32_HOST_OS)
  absPath <- makeAbsolute path
  foldM (\x y -> expandSym $ combinePath x y) "/" $ splitPath absPath
#else
  Win32.getFullPathName . normalise $ path
#endif

-- | Dereferences symbolic links until regular
-- file/directory/something_else appears
expandSym :: FilePath -> IO FilePath
expandSym fpath = do
  -- System.Posix.Files.getFileStatus dereferences symlink before
  -- checking its status, so it's useless here
  deref <- ignoringException (Just <$> readSymbolicLink fpath)
  case deref of
    Just slink -> if isAbsolute slink then expandSym slink
                  else expandSym $ foldl combinePath (takeDirectory fpath) $ splitPath slink
    Nothing -> return fpath

-- | Make a path absolute.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute f
    | not (null f) && head f `elem` ['~', pathSeparator] = return f
    | otherwise = fmap (</> f) getCurrentDirectory

-- | Combines two paths, moves up one level on ..
combinePath :: FilePath -> String -> FilePath
combinePath x "."  = x
combinePath x ".." = takeDirectory x
combinePath x y
    | isDrive x = (x ++ [pathSeparator]) </> y -- "C:" </> "bin" = "C:bin"
    | otherwise = x </> y

replaceUpTo :: Eq a => [a] -> [a] -> [a] -> [a]
replaceUpTo srch rep as =
  case splitOn srch as of
    []      -> []
    [a]     -> a
    (_:as') -> rep ++ last as'

-- replace utility shorthands, similar to Emacs
--   somepath//someotherpath is equivalent to /someotherpath
--   somepath/~/someotherpath is equivalent to ~/someotherpath
replaceShorthands :: FilePath -> FilePath
replaceShorthands = replaceUpTo "/~" "~/" . replaceUpTo "//" "/"

-- | Splits path into parts by path separator
splitPath :: FilePath -> [String]
splitPath = filter (not . null) . splitOneOf pathSeparators
