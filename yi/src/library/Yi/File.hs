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
import           Control.Monad.Reader (asks)
import           Data.Foldable (find)
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time
import           System.Directory
import           System.FilePath
import           System.FriendlyPath
import           Yi.Config
import           Yi.Core
import           Yi.Dired
import           Yi.Monad
import           Yi.Regex
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Utils

-- | If file exists, read contents of file into a new buffer, otherwise
-- creating a new empty buffer. Replace the current window with a new
-- window onto the new buffer.
--
-- If the file is already open, just switch to the corresponding buffer.
--
-- Need to clean up semantics for when buffers exist, and how to attach
-- windows to buffers.
editFile :: FilePath -> YiM BufferRef
editFile filename = do
    f <- io $ userToCanonPath filename

    dupBufs <- filter (maybe False (equalFilePath f) . file) <$> gets bufferSet

    dirExists  <- io $ doesDirectoryExist f
    fileExists <- io $ doesFileExist f

    b <- case dupBufs of
      [] -> if dirExists
               then diredDirBuffer f
               else setupMode f =<< if fileExists
                                       then fileToNewBuffer f
                                       else newEmptyBuffer f
      (h:_) -> return $ bkey h

    withEditor $ switchToBufferE b >> addJumpHereE
    return b
  where
    fileToNewBuffer :: FilePath -> YiM BufferRef
    fileToNewBuffer f = do
      now <- io getCurrentTime
      contents <- io $ R.readFile f

      b <- withEditor $ stringToNewBuffer (FileBuffer f) contents
      withGivenBuffer b $ markSavedB now

      return b

    newEmptyBuffer :: FilePath -> YiM BufferRef
    newEmptyBuffer f =
      withEditor $ stringToNewBuffer (FileBuffer f) mempty

    setupMode :: FilePath -> BufferRef -> YiM BufferRef
    setupMode f b = do
      tbl <- asks (modeTable . yiConfig)
      content <- withGivenBuffer b elemsB

      let header = R.take 1024 content
          rx = "\\-\\*\\- *([^ ]*) *\\-\\*\\-" :: String
          hmode = case R.toString header =~ rx of
              AllTextSubmatches [_,m] -> T.pack m
              _ -> ""
          Just mode = find (\(AnyMode m) -> modeName m == hmode) tbl <|>
                      find (\(AnyMode m) -> modeApplies m f header) tbl <|>
                      Just (AnyMode emptyMode)
      case mode of
          AnyMode newMode -> withGivenBuffer b $ setMode newMode

      return b

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
    (Just f, contents) -> do liftBase $ R.writeFile f contents
                             now <- io getCurrentTime
                             withGivenBuffer bufferKey (markSavedB now)
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
