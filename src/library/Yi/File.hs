{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.File
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.File (
  -- * File-based actions
  editFile,
  openingNewFile,
  openNewFile,

  viWrite, viWriteTo, viSafeWriteTo,
  fwriteE,
  fwriteBufferE,
  fwriteAllY,
  fwriteToE,
  backupE,
  revertE,

  -- * Helper functions
  setFileName,
  deservesSave,

  -- * Configuration
  preSaveHooks
 ) where

import           Control.Applicative    ((<$>))
import           Control.Lens           (assign, makeLenses, use, view, (^.))
import           Control.Monad          (filterM, void, when)
import           Control.Monad.Base     (liftBase)
import           Data.Default           (Default, def)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T (Text, append, cons, pack, unpack)
import           Data.Time              (getCurrentTime)
import           Data.Typeable          (Typeable)
import           System.Directory       (doesDirectoryExist, doesFileExist)
import           System.FriendlyPath    (userToCanonPath)
import           Yi.Buffer
import           Yi.Config.Simple.Types (Field, customVariable)
import           Yi.Core                (errorEditor, runAction)
import           Yi.Dired               (editFile)
import           Yi.Editor
import           Yi.Keymap              ()
import           Yi.Monad               (gets)
import qualified Yi.Rope                as R (readFile, writeFile, writeFileUsingText)
import           Yi.String              (showT)
import           Yi.Types
import           Yi.Utils               (io)

newtype PreSaveHooks = PreSaveHooks { _unPreSaveHooks :: [Action] }
    deriving Typeable

instance Default PreSaveHooks where
    def = PreSaveHooks []

instance YiConfigVariable PreSaveHooks

makeLenses ''PreSaveHooks

preSaveHooks :: Field [Action]
preSaveHooks = customVariable . unPreSaveHooks

-- | Tries to open a new buffer with 'editFile' and runs the given
-- action on the buffer handle if it succeeds.
--
-- If the 'editFile' fails, just the failure message is printed.
openingNewFile :: FilePath -> BufferM a -> YiM ()
openingNewFile fp act = editFile fp >>= \case
  Left m -> printMsg m
  Right ref -> void $ withGivenBuffer ref act

-- | Same as @openingNewFile@ with no action to run after.
openNewFile :: FilePath -> YiM ()
openNewFile = flip openingNewFile $ return ()

-- | Revert to the contents of the file on disk
revertE :: YiM ()
revertE =
  withCurrentBuffer (gets file) >>= \case
    Just fp -> do
      now <- io getCurrentTime
      rf <- liftBase $ R.readFile fp >>= \case
        Left m -> print ("Can't revert: " <> m) >> return Nothing
        Right (c, cv) -> return $ Just (c, Just cv)
      case rf of
       Nothing -> return ()
       Just (s, conv) -> do
         withCurrentBuffer $ revertB s conv now
         printMsg ("Reverted from " <> showT fp)
    Nothing -> printMsg "Can't revert, no file associated with buffer."


-- | Try to write a file in the manner of vi/vim
-- Need to catch any exception to avoid losing bindings
viWrite :: YiM ()
viWrite =
  withCurrentBuffer (gets file) >>= \case
    Nothing -> errorEditor "no file name associated with buffer"
    Just f  -> do
      bufInfo <- withCurrentBuffer bufInfoB
      let s   = bufInfoFileName bufInfo
      succeed <- fwriteE
      let message = (showT f <>) (if f == s
                        then " written"
                        else " " <> showT s <> " written")
      when succeed $ printMsg message

-- | Try to write to a named file in the manner of vi/vim
viWriteTo :: T.Text -> YiM ()
viWriteTo f = do
  bufInfo <- withCurrentBuffer bufInfoB
  let s   = T.pack $ bufInfoFileName bufInfo
  succeed <- fwriteToE f
  let message = f `T.append` if f == s
                             then " written"
                             else ' ' `T.cons` s `T.append` " written"
  when succeed $ printMsg message

-- | Try to write to a named file if it doesn't exist. Error out if it does.
viSafeWriteTo :: T.Text -> YiM ()
viSafeWriteTo f = do
  existsF <- liftBase $ doesFileExist (T.unpack f)
  if existsF
    then errorEditor $ f <> ": File exists (add '!' to override)"
    else viWriteTo f

-- | Write current buffer to disk, if this buffer is associated with a file
fwriteE :: YiM Bool
fwriteE = fwriteBufferE =<< gets currentBuffer

-- | Write a given buffer to disk if it is associated with a file.
fwriteBufferE :: BufferRef -> YiM Bool
fwriteBufferE bufferKey = do
  nameContents <- withGivenBuffer bufferKey $ do
    fl <- gets file
    st <- streamB Forward 0
    conv <- use encodingConverterNameA
    return (fl, st, conv)

  case nameContents of
    (Just f, contents, conv) -> io (doesDirectoryExist f) >>= \case
      True -> printMsg "Can't save over a directory, doing nothing." >> return False
      False -> do
        hooks <- view preSaveHooks <$> askCfg
        mapM_ runAction hooks
        mayErr <- liftBase $ case conv of
          Nothing -> R.writeFileUsingText f contents >> return Nothing
          Just cn -> R.writeFile f contents cn
        case mayErr of
          Just err -> printMsg err >> return False
          Nothing -> io getCurrentTime >>= withGivenBuffer bufferKey . markSavedB
                     >> return True
    (Nothing, _, _) -> printMsg "Buffer not associated with a file" >> return False

-- | Write current buffer to disk as @f@. The file is also set to @f@.
fwriteToE :: T.Text -> YiM Bool
fwriteToE f = do
  b <- gets currentBuffer
  setFileName b (T.unpack f)
  fwriteBufferE b

-- | Write all open buffers
fwriteAllY :: YiM Bool
fwriteAllY = do
    modifiedBuffers <- filterM deservesSave =<< gets bufferSet
    and <$> mapM fwriteBufferE (fmap bkey modifiedBuffers)

-- | Make a backup copy of file
backupE :: FilePath -> YiM ()
backupE = error "backupE not implemented"


-- | Associate buffer with file; canonicalize the given path name.
setFileName :: BufferRef -> FilePath -> YiM ()
setFileName b filename = do
  cfn <- liftBase $ userToCanonPath filename
  withGivenBuffer b $ assign identA $ FileBuffer cfn

-- | Checks if the given buffer deserves a save: whether it's a file
-- buffer and whether it's pointing at a file rather than a directory.
deservesSave :: FBuffer -> YiM Bool
deservesSave b
   | isUnchangedBuffer b = return False
   | otherwise = isFileBuffer b

-- | Is there a proper file associated with the buffer?
-- In other words, does it make sense to offer to save it?
isFileBuffer :: FBuffer -> YiM Bool
isFileBuffer b = case b ^. identA of
  MemBuffer _ -> return False
  FileBuffer fn -> not <$> liftBase (doesDirectoryExist fn)
