{-# LANGUAGE OverloadedStrings #-}
module Yi.File
 (
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
  setFileName,
 ) where

import Control.Applicative
import Control.Monad.Reader (asks)
import Control.Monad.Base
import Control.Lens
import Data.Time
import Data.Foldable (find)
import System.Directory
import System.FilePath
import System.FriendlyPath
import qualified Data.Rope as R

import Yi.Config
import Yi.Core
import Yi.Dired
import Yi.Regex
import Yi.Utils
import Yi.Monad

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

      b <- withEditor $ stringToNewBuffer (Right f) contents
      withGivenBuffer b $ markSavedB now

      return b

    newEmptyBuffer :: FilePath -> YiM BufferRef
    newEmptyBuffer f =
      withEditor $ stringToNewBuffer (Right f) ""

    setupMode :: FilePath -> BufferRef -> YiM BufferRef
    setupMode f b = do
      tbl <- asks (modeTable . yiConfig)
      content <- withGivenBuffer b elemsB

      let header = take 1024 content
          hmode = case header =~ ("\\-\\*\\- *([^ ]*) *\\-\\*\\-"::String) of
              AllTextSubmatches [_,m] ->m
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
            mfp <- withBuffer $ gets file
            case mfp of
                     Just fp -> do
                             now <- io getCurrentTime
                             s <- liftBase $ R.readFile fp
                             withBuffer $ revertB s now
                             msgEditor ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgEditor "Can't revert, no file associated with buffer."
                                return ()

-- | Try to write a file in the manner of vi\/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: YiM ()
viWrite = do
    mf <- withBuffer $ gets file
    case mf of
        Nothing -> errorEditor "no file name associate with buffer"
        Just f  -> do
            bufInfo <- withBuffer bufInfoB
            let s   = bufInfoFileName bufInfo
            fwriteE
            let message = (show f ++) (if f == s
                              then " written"
                              else " " ++ show s ++ " written")
            msgEditor message

-- | Try to write to a named file in the manner of vi\/vim
viWriteTo :: String -> YiM ()
viWriteTo f = do
    bufInfo <- withBuffer bufInfoB
    let s   = bufInfoFileName bufInfo
    fwriteToE f
    let message = (show f ++) (if f == s 
                      then " written"
                      else " " ++ show s ++ " written")
    msgEditor message

-- | Try to write to a named file if it doesn't exist. Error out if it does.
viSafeWriteTo :: String -> YiM ()
viSafeWriteTo f = do
    existsF <- liftBase $ doesFileExist f
    if existsF
       then errorEditor $ f ++ ": File exists (add '!' to override)"
       else viWriteTo f

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = fwriteBufferE =<< gets currentBuffer

-- | Write a given buffer to disk if it is associated with a file.
fwriteBufferE :: BufferRef -> YiM ()
fwriteBufferE bufferKey =
  do nameContents <- withGivenBuffer bufferKey ((,) <$> gets file <*> streamB Forward 0)
     case nameContents of
       (Just f, contents) -> do liftBase $ R.writeFile f contents
                                now <- io getCurrentTime
                                withGivenBuffer bufferKey (markSavedB now)
       (Nothing, _c)      -> msgEditor "Buffer not associated with a file"

-- | Write current buffer to disk as @f@. The file is also set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do
    b <- gets currentBuffer
    setFileName b f
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
  withGivenBuffer b $ assign identA $ Right cfn

