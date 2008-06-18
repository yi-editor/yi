module Yi.File (
        -- * File-based actions
        fwriteE,        -- :: YiM ()
        fwriteBufferE,  -- :: BufferM ()
        fwriteAllE,     -- :: YiM ()
        fwriteToE,      -- :: String -> YiM ()
        backupE,        -- :: FilePath -> YiM ()

        -- * Buffer editing
        revertE,        -- :: YiM ()
) where


import Control.Monad.State (gets)
import Control.Monad.Trans
import Prelude hiding (error)
import System.FilePath
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Editor
  ( getBuffers, getBuffer )
import Yi.Core
import Yi.Debug
import Yi.Keymap
import System.IO.UTF8 as UTF8


-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
            mfp <- withBuffer getfileB
            case mfp of
                     Just fp -> do
                             s <- liftIO $ UTF8.readFile fp
                             withBuffer $ do
                                  savingPointB $ replaceBufferContent s 
                                  markSavedB
                             msgEditor ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgEditor "Can't revert, no file associated with buffer."
                                return ()

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = fwriteBufferE =<< withEditor getBuffer

-- | Write a given buffer to a disk if it is associated with a file.
fwriteBufferE :: BufferRef -> YiM ()
fwriteBufferE bufferKey = 
  do nameContents <- withGivenBuffer bufferKey getNameAndContents
     case nameContents of
       (Just f, contents) -> do withGivenBuffer bufferKey markSavedB
                                liftIO $ UTF8.writeFile f contents
       (Nothing, _c)      -> msgEditor "Buffer not associated with a file"
  where

  getNameAndContents :: BufferM (Maybe FilePath, String)
  getNameAndContents =
    do contents <- elemsB
       fname    <- getfileB
       return (fname, contents)

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do withBuffer $ setfileB f
                 fwriteE

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE = 
  do buffers     <- withEditor getBuffers
     let modifiedBuffers = filter (not . isUnchangedBuffer) buffers
     mapM_ fwriteBufferE (map bkey modifiedBuffers)

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"
