{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeOperators #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy
-- | Various high-level functions to further classify.
module Yi.Misc
where

{- Standard Library Module Imports -}
import Data.List
  ( isPrefixOf
  , (\\)
  , filter
  )
import System.FriendlyPath
  ( expandTilda
  , isAbsolute'
  )
import System.FilePath
  ( takeDirectory
  , (</>)
  , addTrailingPathSeparator
  , hasTrailingPathSeparator
  , takeFileName
  )
import System.Directory
  ( doesDirectoryExist
  , getDirectoryContents
  , getCurrentDirectory
  )

import Control.Monad.Trans (MonadIO (..))
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Prelude ()
import Yi.Core

import Yi.MiniBuffer
    ( simpleComplete
    , withMinibufferGen
    )
import System.CanonicalizePath (canonicalizePath)

-- | Given a possible starting path (which if not given defaults to
--   the current directory) and a fragment of a path we find all
--   files within the given (or current) directory which can complete
--   the given path fragment.
--   We return a pair of both directory plus the filenames on their own
--   that is without their directories. The reason for this is that if
--   we return all of the filenames then we get a 'hint' which is way too
--   long to be particularly useful.
getAppropriateFiles :: Maybe String -> String -> YiM (String, [ String ])
getAppropriateFiles start s = do
  curDir <- case start of
            Nothing -> do bufferPath <- withBuffer $ gets file
                          liftIO $ getFolder bufferPath
            (Just path) -> return path
  let sDir = if hasTrailingPathSeparator s then s else takeDirectory s
      searchDir = if null sDir then curDir
                  else if isAbsolute' sDir then sDir
                  else curDir </> sDir
  searchDir' <- liftIO $ expandTilda searchDir
  let fixTrailingPathSeparator f = do
                       isDir <- doesDirectoryExist (searchDir' </> f)
                       return $ if isDir then addTrailingPathSeparator f else f
  files <- liftIO $ getDirectoryContents searchDir'
  -- Remove the two standard current-dir and parent-dir as we do not
  -- need to complete or hint about these as they are known by users.
  let files' = files \\ [ ".", ".." ]
  fs <- liftIO $ mapM fixTrailingPathSeparator files'
  let matching = filter (isPrefixOf $ takeFileName s) fs
  return (sDir, matching)

-- | Given a path, trim the file name bit if it exists.  If no path
--   given, return current directory.
getFolder :: Maybe String -> IO String
getFolder Nothing     = getCurrentDirectory
getFolder (Just path) = do
  isDir <- doesDirectoryExist path
  let dir = if isDir then path else takeDirectory path
  if null dir then getCurrentDirectory else return dir


-- | Given a possible path and a prefix, return matching file names.
matchingFileNames :: Maybe String -> String -> YiM [String]
matchingFileNames start s = do
  (sDir, files) <- getAppropriateFiles start s
  return $ fmap (sDir </>) files

adjBlock :: Int -> BufferM ()
adjBlock x = withSyntaxB' (\m s -> modeAdjustBlock m s x)

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> BufferM ()
adjIndent ib = withSyntaxB' (\m s -> modeIndent m s ib)



-- | Generic emacs style prompt file action. Takes a @prompt and a continuation @act
--   and prompts the user with file hints
promptFile :: String -> (String -> YiM ()) -> YiM ()
promptFile prompt act = do maybePath <- withBuffer $ gets file
                           startPath <- addTrailingPathSeparator <$> (liftIO $ canonicalizePath =<< getFolder maybePath)
                           -- TODO: Just call withMinibuffer
                           withMinibufferGen startPath (findFileHint startPath) prompt (simpleComplete $ matchingFileNames (Just startPath)) act


-- | For use as the hint when opening a file using the minibuffer.
-- We essentially return all the files in the given directory which
-- have the given prefix.
findFileHint :: String -> String -> YiM [String]
findFileHint startPath s = snd <$> getAppropriateFiles (Just startPath) s
