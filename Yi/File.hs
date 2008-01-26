module Yi.File (
        -- * File-based actions
        fwriteE,        -- :: YiM ()
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
import Yi.Core
import Yi.Debug
import Yi.Keymap


-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
            mfp <- withBuffer getfileB
            case mfp of
                     Just fp -> do
                             s <- liftIO $ readFile fp
                             withBuffer $ do
                                  end <- sizeB
                                  p <- pointB
                                  moveTo 0
                                  deleteN end
                                  insertN s
                                  moveTo p
                                  clearUndosB
                             msgE ("Reverted from " ++ show fp)
                     Nothing -> do
                                msgE "Can't revert, no file associated with buffer."
                                return ()

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM ()
fwriteE = do contents <- withBuffer elemsB
             fname <- withBuffer (gets file)
             withBuffer clearUndosB
             case fname of
               Just n -> liftIO $ writeFile n contents
               Nothing -> msgE "Buffer not associated with a file."

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do withBuffer $ setfileB f
                 fwriteE

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE = error "fwriteAllE not implemented"

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"
