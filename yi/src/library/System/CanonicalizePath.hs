{-# LANGUAGE CPP #-}

-- | System.Directory.canonicalizePath replacement
module System.CanonicalizePath
  ( canonicalizePath
  , normalisePath
  , replaceShorthands
  ) where

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif

import Control.Applicative
import Control.Monad
import Data.List.Split     (splitOn, splitOneOf)
import System.FilePath     ((</>), isAbsolute, takeDirectory, pathSeparator, pathSeparators)
import System.Directory    (getCurrentDirectory)
import System.Posix.Files  (readSymbolicLink)


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
  

-- | Make a path absolute.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute f
    | not (null f) && head f `elem` ['~', pathSeparator] = return f
    | otherwise = fmap (</> f) getCurrentDirectory

-- | Combines two paths, moves up one level on ..
combinePath :: FilePath -> String -> FilePath
combinePath x "."  = x
combinePath x ".." = takeDirectory x
combinePath x y = x </> y

replaceUpTo :: Eq a => [a] -> [a] -> [a] -> [a]
replaceUpTo srch rep as =
  case splitOn srch as of
    [] -> []
    [a] -> a
    (a:as) -> rep ++ last as

-- replace utility shorthands, similar to Emacs
--   somepath//someotherpath is equivalent to /someotherpath
--   somepath/~/someotherpath is equivalent to ~/someotherpath
replaceShorthands :: FilePath -> FilePath
replaceShorthands = replaceUpTo "/~" "~" . replaceUpTo "//" "/" 

-- | Splits path into parts by path separator
splitPath :: FilePath -> [String]
splitPath = filter (not . null) . splitOneOf pathSeparators
