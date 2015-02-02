{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  System.CanonicalizePath
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- System.Directory.canonicalizePath replacement
module System.CanonicalizePath
  ( canonicalizePath
  , normalisePath
  , replaceShorthands
  ) where


#ifdef mingw32_HOST_OS
import           System.FilePath          (normalise)
import qualified System.Win32             as Win32
#endif

import           Control.Applicative      ((<$>))
import           Control.Exc              (ignoringException)
import           Control.Monad            (foldM)
import           Data.List.Split          (splitOneOf)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T (Text, empty, splitOn)
import           System.Directory         (getCurrentDirectory)
import           System.FilePath          (isAbsolute, isDrive, pathSeparator,
                                           pathSeparators, takeDirectory, (</>))
import           System.PosixCompat.Files (readSymbolicLink)

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
    Just slink -> expandSym (if isAbsolute slink
                    then slink
                    else foldl combinePath (takeDirectory fpath) $ splitPath slink)
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
combinePath "/" y  = "/" </> y
combinePath x y
    | isDrive x = (x ++ [pathSeparator]) </> y -- "C:" </> "bin" = "C:bin"
    | otherwise = x </> y

-- Replace utility shorthands, similar to Emacs
--
-- @
-- somepath//someotherpath  ≅ /someotherpath
-- somepath/~/someotherpath ≅ ~/someotherpath
-- @
replaceShorthands :: T.Text -> T.Text
replaceShorthands = r "/~" "~/" . r "//" "/"
  where
    r :: T.Text -> T.Text -> T.Text -> T.Text
    r s r' a = case T.splitOn s a of
      []     -> T.empty
      [a']   -> a'
      _ : as -> r' <> last as

-- | Splits path into parts by path separator
--
-- Text version would look like
--
-- @'T.filter' (not . T.null) . T.split (`elem` pathSeparators)@
--
-- But we should move to @system-filepath@ package anyway.
splitPath :: FilePath -> [String]
splitPath = filter (not . null) . splitOneOf pathSeparators
