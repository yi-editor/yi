{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
module Yi.GHC where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Error ()
import Control.Monad.Reader (asks)
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import ErrUtils
import GHC
import Outputable
import Prelude ()
import Shim.Hsinfo
import Shim.SHM hiding (io)
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap
import Yi.Prelude hiding ((<>))
import qualified Data.List.PointedList.Circular as PL
import qualified Data.Map as M
import qualified Yi.Editor as Editor

newShim :: YiM (MVar ShimState)
newShim = do
    session <- io $ ghcInit

    r <- asks yiVar
    cfg <- asks yiConfig
    let logMsg msgSeverity msgSrcSpan style msg =
           unsafeWithEditor cfg r $ do
             let note = CompileNote msgSeverity msgSrcSpan style msg
             modA notesA (Just . maybe (PL.singleton note) (PL.insertRight note))
             printMsg ('\n':show ((mkLocMessage msgSrcSpan msg) style))


    io $ newMVar ShimState
               { tempSession = session,
                 sessionMap = M.empty,
                 compBuffer = M.empty,
                 compLogAction = logMsg }


getShim :: YiM (MVar ShimState)
getShim = do
  r <- withEditor $ getA maybeShimA
  case r of
    Just x -> return x
    Nothing -> do x <- newShim
                  withEditor $ putA maybeShimA (Just x)
                  return x

withShim :: SHM a -> YiM a
withShim f = do
  r <- getShim
  liftIO $ do e <- takeMVar r
              (a,e') <- runStateT f e
              putMVar r e'
              return a

runShimThread :: SHM () -> YiM ThreadId
runShimThread f = do
  r <- getShim
  (liftIO . forkOS) $
           do e <- takeMVar r
              (a,e') <- runStateT f e
              putMVar r e'
              return a

maybeShimA  :: Accessor Editor (Maybe (MVar ShimState))
maybeShimA = dynamicValueA . dynamicA

-- ---------------------------------------------------------------------
-- CompileNote

-- TODO: Perhaps this CompileNote should be replaced with something
-- more general not necessary GHC specific.
--
-- It is moved here from Yi.Mode.Shim because it has to be accessible
-- from Yi.Core. The CompileNote type itself comes from SHIM

data CompileNote = CompileNote {severity :: Severity,
                                srcSpan :: SrcSpan,
                                pprStyle :: PprStyle,
                                message :: Message}


instance Show CompileNote where
    show n = show $
               (hang (ppr (srcSpan n) <> colon) 4  (message n)) (pprStyle n)


type T = (Maybe (PL.PointedList CompileNote))
newtype ShimNotes = ShimNotes { fromShimNotes :: T }
    deriving Typeable

instance Default ShimNotes where
    def = ShimNotes Nothing



notesA :: Accessor Editor T
notesA =  (accessor fromShimNotes (\x (ShimNotes _) -> ShimNotes x))
          . dynamicValueA . dynamicA


