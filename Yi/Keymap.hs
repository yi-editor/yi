{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

-- Copyright (c) Jean-Philippe Bernardy 2007.

module Yi.Keymap where

import Prelude hiding (error)
import Yi.UI.Common
import qualified Yi.Editor as Editor
import Yi.Editor (EditorM, Editor, runEditor)
import qualified Data.Map as M
import Control.Monad.Reader
import Data.Typeable
import Data.IORef
import Control.Exception
import Control.Concurrent
import Yi.Buffer
import qualified Yi.Interact as I
import Yi.Monad
import Control.Monad.State
import Yi.Event
import Yi.Syntax
import Yi.Process ( SubprocessInfo, SubprocessId )
import qualified Yi.UI.Common as UI
import Data.Dynamic
import Data.Typeable
#ifdef SHIM
import Shim.SHM
#endif

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
--            | InsertA String
--             | TextA Direction Unit Operation
        deriving Typeable
instance I.PEq Action where
    equiv _ _ = False

instance Show Action where
    show (YiA _) = "@Y"
    show (EditorA _) = "@E"
    show (BufferA _) = "@B"

type Interact ev a = I.I ev Action a

type KeymapM a = Interact Event a

type Keymap = KeymapM ()

type KeymapEndo = Keymap -> Keymap

type KeymapProcess = I.P Event Action


data Config = Config {startFrontEnd :: UI.UIBoot,
                      configUI :: UIConfig,
                      startAction :: YiM (),
                      startQueuedActions :: [Action], -- ^ for performance testing
                      defaultKm :: Keymap,                      
                      modeTable :: ReaderT String Maybe Mode,
                      fundamentalMode :: Mode,
                      publishedActions :: M.Map String [Dynamic]}


data Yi = Yi {yiEditor :: IORef Editor,
              yiUi          :: UI,
              threads       :: IORef [ThreadId],           -- ^ all our threads
              input         :: Event -> IO (),             -- ^ input stream
              output        :: Action -> IO (),            -- ^ output stream
              defaultKeymap :: IORef Keymap, -- TODO: remove this in favour of using the yiConfig one.
              bufferProcesses :: IORef (M.Map BufferRef KeymapProcess), -- FIXME: should be an MVar (can be accessed both by worker and input thread)
              yiSubprocessIdSupply :: IORef SubprocessId,
              yiSubprocesses :: IORef (M.Map SubprocessId SubprocessInfo),
              yiConfig :: Config
#ifdef SHIM
              ,yiShim :: IORef ShimState
#endif
             }
             deriving Typeable

-- | The type of user-bindable functions
newtype YiM a = YiM {runYiM :: ReaderT Yi IO a}
    deriving (Monad, MonadReader Yi, MonadIO, Typeable)


-----------------------
-- Keymap basics

-- | @write a@ returns a keymap that just outputs the action @a@.
write :: (I.MonadInteract m Action ev, YiAction a x, Show x) => a -> m ()
write x = I.write (makeAction x)


-----------------------
-- Keymap thread handling


-- FIXME: we never cleanup buffer processes
setBufferMode :: BufferRef -> Mode -> YiM ()
setBufferMode b m = do
  withGivenBuffer b $ setMode m
  restartBufferThread b

restartBufferThread :: BufferRef -> YiM ()
restartBufferThread b = do
  modifiesRef bufferProcesses (M.insert b I.End)

getBufferMode :: BufferRef -> YiM Mode
getBufferMode b = withGivenBuffer b $ gets bmode

getBufferProcess :: BufferRef -> YiM KeymapProcess
getBufferProcess b = do
  kms <- readsRef bufferProcesses
  return $ case M.lookup b kms of
    Just bkm -> bkm
    Nothing -> I.End


--------------------------------
-- Uninteresting glue code

withUI :: (UI -> IO a) -> YiM a
withUI = with yiUi

withEditor :: EditorM a -> YiM a
withEditor f = do
  r <- asks yiEditor
  e <- readRef r
  let (a,e') = runEditor f e
  -- Make sure that the result of runEditor is evaluated before
  -- replacing the editor state. Otherwise, we might replace e
  -- with an exception-producing thunk, which makes it impossible
  -- to look at or update the editor state.
  -- Maybe this could also be fixed by -fno-state-hack flag?
  e' `seq` a `seq` writeRef r e'
  return a

withGivenBuffer :: BufferRef -> BufferM a -> YiM a
withGivenBuffer b f = withEditor (Editor.withGivenBuffer0 b f)

withBuffer :: BufferM a -> YiM a
withBuffer f = withEditor (Editor.withBuffer0 f)

#ifdef SHIM
withShim :: SHM a -> YiM a
withShim f = do
  r <- asks yiShim
  liftIO $ do e <- readRef r
              (a,e') <- runStateT f e
              writeRef r e'
              return a
#endif

readEditor :: (Editor -> a) -> YiM a
readEditor f = withEditor (gets f)

catchDynE :: Typeable exception => YiM a -> (exception -> YiM a) -> YiM a
catchDynE (YiM inner) handler
    = YiM $ ReaderT (\r -> catchDyn (runReaderT inner r) (\e -> runReaderT (runYiM $ handler e) r))

catchJustE :: (Exception -> Maybe b) -- ^ Predicate to select exceptions
           -> YiM a      -- ^ Computation to run
           -> (b -> YiM a) -- ^   Handler
           -> YiM a
catchJustE p (YiM c) h = YiM $ ReaderT (\r -> catchJust p (runReaderT c r) (\b -> runReaderT (runYiM $ h b) r))

handleJustE :: (Exception -> Maybe b) -> (b -> YiM a) -> YiM a -> YiM a
handleJustE p h c = catchJustE p c h

-- | Shut down all of our threads. Should free buffers etc.
shutdown :: YiM ()
shutdown = do ts <- readsRef threads
              liftIO $ mapM_ killThread ts

-- -------------------------------------------

class YiAction a x | a -> x where
    makeAction :: Show x => a -> Action

instance YiAction (YiM x) x where
    makeAction = YiA


instance YiAction (EditorM x) x where
    makeAction = EditorA

instance YiAction (BufferM x) x where
    makeAction = BufferA
