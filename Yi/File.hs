module Yi.File 
 (
  -- * File-based actions
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

import Data.Time
import Control.Monad.Trans
import Prelude (filter)
import Yi.Core
import Yi.Buffer (file)
import Control.Monad.State (gets)
import System.Directory
import System.IO.UTF8 as UTF8
import System.FilePath
import System.FriendlyPath
import qualified Data.ByteString.Lazy as LB

-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
            mfp <- withBuffer $ gets file
            case mfp of
                     Just fp -> do
                             now <- io getCurrentTime
                             s <- liftIO $ UTF8.readFile fp
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
            msgEditor $ show f ++ " " ++ show s ++ " written"

-- | Try to write to a named file in the manner of vi\/vim
viWriteTo :: String -> YiM ()
viWriteTo f = do
    bufInfo <- withBuffer bufInfoB
    let s   = bufInfoFileName bufInfo
    fwriteToE f
    msgEditor $ show f++" "++show s ++ " written"

-- | Try to write to a named file if it doesn't exist. Error out if it does.
viSafeWriteTo :: String -> YiM ()
viSafeWriteTo f = do
    existsF <- liftIO $ doesFileExist f
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
     now <- io getCurrentTime
     case nameContents of
       (Just f, contents) -> do withGivenBuffer bufferKey (markSavedB now)
                                liftIO $ LB.writeFile f contents 
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
  cfn <- liftIO $ canonicalizePath =<< expandTilda filename
  withGivenBuffer b $ putA identA $ Right cfn

