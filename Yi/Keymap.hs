{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies #-}

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

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
--            | InsertA String
--             | TextA Direction Unit Operation

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
                      startAction :: YiM (),
                      startQueuedActions :: [Action], -- ^ for performance testing
                      defaultKm :: Keymap,                      
                      modeTable :: String -> Maybe Mode,
                      fundamentalMode :: Mode,
                      publishedActions :: M.Map String Action}

data Mode = Mode 
    {
     -- modeName = "fundamental", -- ^ so this could be serialized, debugged.
     modeHL :: ExtHL,
     modeKeymap :: KeymapEndo, -- ^ Buffer's local keymap modification
     modeIndent :: BufferM ()
    }

data Yi = Yi {yiEditor :: IORef Editor,
              yiUi          :: UI,
              threads       :: IORef [ThreadId],           -- ^ all our threads
              input         :: Chan Event,                 -- ^ input stream
              output        :: Chan Action,                -- ^ output stream
              defaultKeymap :: IORef Keymap, -- TODO: remove this in favour of using the yiConfig one.
              bufferMode    :: IORef (M.Map BufferRef Mode), -- FIXME: mode should be stored in the Buffer, but this would create a circular dependency.
              bufferProcesses :: IORef (M.Map BufferRef KeymapProcess), -- FIXME: should be an MVar (can be accessed both by worker and input thread)
              editorModules :: IORef [String], -- ^ modules requested by user: (e.g. ["YiConfig", "Yi.Dired"]) DEPRECATED

              yiSubprocessIdSource :: IORef SubprocessId,
              yiSubprocesses :: IORef (M.Map SubprocessId SubprocessInfo),
              yiConfig :: Config
             }

-- | The type of user-bindable functions
type YiM = ReaderT Yi IO

-----------------------
-- Keymap basics

-- | @write a@ returns a keymap that just outputs the action @a@.
write :: (I.MonadInteract m Action ev, YiAction a x, Show x) => a -> m ()
write x = I.write (makeAction x)


-----------------------
-- Keymap thread handling


-- FIXME: we never cleanup buffer mode/processes
setBufferMode :: BufferRef -> Mode -> YiM ()
setBufferMode b m = do
  modifiesRef bufferMode (M.insert b m)
  withGivenBuffer b $ setSyntaxB (modeHL m)
  restartBufferThread b

restartBufferThread :: BufferRef -> YiM ()
restartBufferThread b = do
  modifiesRef bufferProcesses (M.insert b I.End)

getBufferMode :: BufferRef -> YiM Mode
getBufferMode b = do
  kms <- readsRef bufferMode
  case M.lookup b kms of
    Just bkm -> return bkm
    Nothing -> asks (fundamentalMode . yiConfig)

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

readEditor :: (Editor -> a) -> YiM a
readEditor f = withEditor (gets f)

catchDynE :: Typeable exception => YiM a -> (exception -> YiM a) -> YiM a
catchDynE inner handler = ReaderT (\r -> catchDyn (runReaderT inner r) (\e -> runReaderT (handler e) r))

catchJustE :: (Exception -> Maybe b) -- ^ Predicate to select exceptions
           -> YiM a      -- ^ Computation to run
           -> (b -> YiM a) -- ^   Handler
           -> YiM a
catchJustE p c h = ReaderT (\r -> catchJust p (runReaderT c r) (\b -> runReaderT (h b) r))

handleJustE :: (Exception -> Maybe b) -> (b -> YiM a) -> YiM a -> YiM a
handleJustE p h c = catchJustE p c h

-- | Shut down all of our threads. Should free buffers etc.
shutdown :: YiM ()
shutdown = do ts <- readsRef threads
              lift $ mapM_ killThread ts

-- -------------------------------------------

class YiAction a x | a -> x where
    makeAction :: Show x => a -> Action

instance YiAction (YiM x) x where
    makeAction = YiA


instance YiAction (EditorM x) x where
    makeAction = EditorA

instance YiAction (BufferM x) x where
    makeAction = BufferA
