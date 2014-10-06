{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.File
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.File (
  -- * File-based actions
  editFile,       -- :: YiM BufferRef

  viWrite, viWriteTo, viSafeWriteTo,
  fwriteE,        -- :: YiM ()
  fwriteBufferE,  -- :: BufferM ()
  fwriteAllE,     -- :: YiM ()
  fwriteToE,      -- :: String -> YiM ()
  backupE,        -- :: FilePath -> YiM ()
  revertE,        -- :: YiM ()

  -- * Helper functions
  setFileName
 ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Base
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           System.Directory
import           System.FriendlyPath
import           Yi.Core
import           Yi.Dired
import           Yi.Monad
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Utils

-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
  withBuffer (gets file) >>= \case
    Just fp -> do
      now <- io getCurrentTime
      s <- liftBase $ R.readFile fp
      withBuffer $ revertB s now
      msgEditor ("Reverted from " <> showT fp)
    Nothing -> msgEditor "Can't revert, no file associated with buffer."


-- | Try to write a file in the manner of vi\/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: YiM ()
viWrite = do
  withBuffer (gets file) >>= \case
   Nothing -> errorEditor "no file name associate with buffer"
   Just f  -> do
       bufInfo <- withBuffer bufInfoB
       let s   = bufInfoFileName bufInfo
       fwriteE
       let message = (showT f <>) (if f == s
                         then " written"
                         else " " <> showT s <> " written")
       msgEditor message

-- | Try to write to a named file in the manner of vi/vim
viWriteTo :: T.Text -> YiM ()
viWriteTo f = do
  bufInfo <- withBuffer bufInfoB
  let s   = T.pack $ bufInfoFileName bufInfo
  fwriteToE f
  let message = f `T.append` if f == s
                             then " written"
                             else ' ' `T.cons` s `T.append` " written"
  msgEditor message

-- | Try to write to a named file if it doesn't exist. Error out if it does.
viSafeWriteTo :: T.Text -> YiM ()
viSafeWriteTo f = do
  existsF <- liftBase $ doesFileExist (T.unpack f)
  if existsF
    then errorEditor $ f <> ": File exists (add '!' to override)"
    else viWriteTo f

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = fwriteBufferE =<< gets currentBuffer

-- | Write a given buffer to disk if it is associated with a file.
fwriteBufferE :: BufferRef -> YiM ()
fwriteBufferE bufferKey = do
  nameContents <- withGivenBuffer bufferKey ((,) <$> gets file
                                                 <*> streamB Forward 0)
  case nameContents of
    (Just f, contents) -> io (doesDirectoryExist f) >>= \case
      True -> msgEditor "Can't save over a directory, doing nothing."
      False -> do
        liftBase $ R.writeFile f contents
        io getCurrentTime >>= withGivenBuffer bufferKey . markSavedB
    (Nothing, _c)      -> msgEditor "Buffer not associated with a file"

-- | Write current buffer to disk as @f@. The file is also set to @f@.
fwriteToE :: T.Text -> YiM ()
fwriteToE f = do
  b <- gets currentBuffer
  setFileName b (T.unpack f)
  fwriteBufferE b

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE =
  do allBuffs <- gets bufferSet
     let modifiedBuffers = filter (not . isUnchangedBuffer) allBuffs
     mapM_ fwriteBufferE (fmap bkey modifiedBuffers)

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"


-- | Associate buffer with file; canonicalize the given path name.
setFileName :: BufferRef -> FilePath -> YiM ()
setFileName b filename = do
  cfn <- liftBase $ userToCanonPath filename
  withGivenBuffer b $ assign identA $ FileBuffer cfn
