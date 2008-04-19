{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
module Yi.GHC where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Error ()
import Control.Monad.Reader (asks)
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.Typeable
import ErrUtils
import GHC
import Outputable
import Prelude ()
import Shim.Hsinfo
import Shim.SHM hiding (io)
import System.Directory ( findExecutable )
import Yi.Accessor
import Yi.Dynamic
import Yi.Editor
import Yi.Keymap
import Yi.Prelude
import Yi.WindowSet (WindowSet)
import qualified Data.Map as M
import qualified Yi.Editor as Editor
import qualified Yi.WindowSet as Robin
import qualified Yi.WindowSet as WS

newShim :: YiM (MVar ShimState)
newShim = do
    ghc <- io $ findExecutable "ghc" -- FIXME: Add version constraint
                >>= maybe (error "Could not find ghc executable in path.") return
    session <- io $ ghcInit ghc

    r <- asks yiEditor

    let logMsg severity srcSpan style msg = 
           unsafeWithEditor r $ do                             
             let note = CompileNote severity srcSpan style msg
             modifyA notesA (Just . maybe (WS.new note) (WS.add note))
             printMsg ('\n':show ((mkLocMessage srcSpan msg) style))


    io $ newMVar ShimState
               { ghcProgram = ghc,
                 tempSession = session,
                 sessionMap = M.empty,
                 compBuffer = M.empty,
                 compLogAction = logMsg }


getShim :: YiM (MVar ShimState)
getShim = do
  r <- withEditor $ getA maybeShimA
  case r of
    Just x -> return x
    Nothing -> do x <- newShim
                  withEditor $ setA maybeShimA (Just x)
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
maybeShimA = dynamicValueA .> dynamicA

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


type T = (Maybe (Robin.WindowSet CompileNote))
newtype ShimNotes = ShimNotes { fromShimNotes :: T }
    deriving Typeable
instance Initializable ShimNotes where
    initial = ShimNotes Nothing



notesA :: Accessor Editor T
notesA =  (Accessor fromShimNotes (\f (ShimNotes x) -> ShimNotes (f x))) 
          .> dynamicValueA .> dynamicA 


