module Yi.File 
 (
  -- * File-based actions
  fwriteE,        -- :: YiM ()
  fwriteBufferE,  -- :: BufferM ()
  fwriteAllE,     -- :: YiM ()
  fwriteToE,      -- :: String -> YiM ()
  backupE,        -- :: FilePath -> YiM ()
  revertE,        -- :: YiM ()

  -- * Helper functions
  bestNewName,
  setFileName,
 ) where

import Control.Monad.Trans
import Prelude (filter)
import Yi.Core
import System.Directory
import System.IO.UTF8 as UTF8
import System.FilePath
import System.FriendlyPath
import qualified Data.ByteString.Lazy as LB

-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE = do
            mfp <- withBuffer $ getA fileA
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

-- | Write a given buffer to disk if it is associated with a file.
fwriteBufferE :: BufferRef -> YiM ()
fwriteBufferE bufferKey = 
  do nameContents <- withGivenBuffer bufferKey ((,) <$> getA fileA <*> streamB Forward 0)
     case nameContents of
       (Just f, contents) -> do withGivenBuffer bufferKey markSavedB
                                liftIO $ LB.writeFile f contents 
       (Nothing, _c)      -> msgEditor "Buffer not associated with a file"

-- | Write current buffer to disk as @f@. If this buffer doesn't
-- currently have a file associated with it, the file is set to @f@
fwriteToE :: String -> YiM ()
fwriteToE f = do 
    b <- withEditor $ do
        b <- getBuffer
        currentBufferNames <- fmap name <$> getBuffers
        withBuffer0 $ setA nameA (bestNewName (takeFileName f) currentBufferNames)
        return b
    setFileName b f
    fwriteBufferE b
    

-- | Write all open buffers
fwriteAllE :: YiM ()
fwriteAllE = 
  do buffers <- withEditor getBuffers
     let modifiedBuffers = filter (not . isUnchangedBuffer) buffers
     mapM_ fwriteBufferE (fmap bkey modifiedBuffers)

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"


-- | Associate buffer with file; canonicalize the given path name.
setFileName :: BufferRef -> FilePath -> YiM ()
setFileName b filename = do
  cfn <- liftIO $ canonicalizePath =<< expandTilda filename
  withGivenBuffer b $ setA fileA $ Just cfn

-- Given the desired buffer name, plus a list of current buffer
-- names returns the best name for the new buffer. This will
-- be the desired one in the case that it doesn't currently exist.
-- Otherwise we will suffix it with <n> where n is one more than the
-- current number of suffixed similar names.
-- IOW if we want "file.hs" but one already exists then we'll create
-- "file.hs<1>" but if that already exists then we'll create "file.hs<2>"
-- and so on.
bestNewName :: String -> [String] -> String
bestNewName desiredBufferName currentBufferNames
  | elem desiredBufferName currentBufferNames = addSuffixBName 1
  | otherwise                                 = desiredBufferName
  where
  addSuffixBName :: Int -> String
  addSuffixBName i
    | elem possibleName currentBufferNames = addSuffixBName (i + 1)
    | otherwise                            = possibleName
    where
    possibleName = concat [ desiredBufferName
                          , "<"
                          , show i
                          , ">"
                          ]

