{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving, Rank2Types, TemplateHaskell #-}

-- Copyright (c) Jean-Philippe Bernardy 2007,8.

module Yi.Keymap
    ( Action(..)
    , emptyAction
    , Interact
    , KeymapM
    , Keymap
    , KeymapEndo
    , KeymapProcess
    , KeymapSet(..)
    , topKeymapA
    , insertKeymapA
    , extractTopKeymap
    , modelessKeymapSet
    , YiM(..)
    , withUI
    , unsafeWithEditor
    , withGivenBuffer
    , withBuffer
    , readEditor
    , catchDynE
    , catchJustE
    , handleJustE
    , shutdown
    , YiAction (..)
    , Yi(..)
    , YiVar(..)
    , write
    , withModeY
    ) where

import Control.Concurrent
import Control.Applicative
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.State hiding (mapM_)
import Control.Monad.Base
import Control.Exception
import Data.Typeable
import Yi.Buffer
import Yi.Config
import Yi.Editor (EditorM, Editor, runEditor, MonadEditor(..))
import Yi.Event
import Yi.Monad
import Yi.Process (SubprocessInfo, SubprocessId)
import Yi.UI.Common
import Yi.Utils
import qualified Data.Map as M
import qualified Yi.Editor as Editor
import qualified Yi.Interact as I

-- TODO: refactor this!

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
        deriving Typeable

emptyAction :: Action
emptyAction = BufferA (return ())

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

data Yi = Yi {yiUi          :: UI,
              input         :: Event -> IO (),      -- ^ input stream
              output        :: [Action] -> IO (),   -- ^ output stream
              yiConfig      :: Config,
              -- TODO: this leads to anti-patterns and seems like one itself
              -- too coarse for actual concurrency, otherwise pointless
              -- And MVars can be empty so this causes soundness problems
              -- Also makes code a bit opaque
              yiVar         :: MVar YiVar           -- ^ The only mutable state in the program
             }
             deriving Typeable

data YiVar = YiVar {yiEditor             :: !Editor,
                    threads              :: ![ThreadId],           -- ^ all our threads
                    yiSubprocessIdSupply :: !SubprocessId,
                    yiSubprocesses       :: !(M.Map SubprocessId SubprocessInfo)
                   }

-- | The type of user-bindable functions
-- TODO: doc how these are actually user-bindable
-- are they?
newtype YiM a = YiM {runYiM :: ReaderT Yi IO a}
    deriving (Monad, Applicative, MonadReader Yi, MonadBase IO, Typeable, Functor)

instance MonadState Editor YiM where
    get = yiEditor <$> (readRef =<< yiVar <$> ask)
    put v = flip modifyRef (\x -> x {yiEditor = v}) =<< yiVar <$> ask

instance MonadEditor YiM where
    askCfg = yiConfig <$> ask
    withEditor f = do
      r <- asks yiVar
      cfg <- asks yiConfig
      liftBase $ unsafeWithEditor cfg r f

-----------------------
-- Keymap basics

-- | @write a@ returns a keymap that just outputs the action @a@.
write :: (I.MonadInteract m Action ev, YiAction a x, Show x) => a -> m ()
write x = I.write (makeAction x)

--------------------------------
-- Uninteresting glue code

withUI :: (UI -> IO a) -> YiM a
withUI = with yiUi

unsafeWithEditor :: Config -> MVar YiVar -> EditorM a -> IO a
unsafeWithEditor cfg r f = modifyMVar r $ \var -> do
  let e = yiEditor var
  let (e',a) = runEditor cfg f e
  -- Make sure that the result of runEditor is evaluated before
  -- replacing the editor state. Otherwise, we might replace e
  -- with an exception-producing thunk, which makes it impossible
  -- to look at or update the editor state.
  -- Maybe this could also be fixed by -fno-state-hack flag?
  -- TODO: can we simplify this?
  e' `seq` a `seq` return (var {yiEditor = e'}, a)

withGivenBuffer :: MonadEditor m => BufferRef -> BufferM a -> m a
withGivenBuffer b f = withEditor (Editor.withGivenBuffer0 b f)

withBuffer :: MonadEditor m => BufferM a -> m a
withBuffer f = withEditor (Editor.withBuffer0 f)

readEditor :: MonadEditor m => (Editor -> a) -> m a
readEditor f = withEditor (gets f)

catchDynE :: Exception exception => YiM a -> (exception -> YiM a) -> YiM a
catchDynE (YiM inner) handler
    = YiM $ ReaderT (\r -> catch (runReaderT inner r) (\e -> runReaderT (runYiM $ handler e) r))

catchJustE :: (Exception e) => (e -> Maybe b) -- ^ Predicate to select exceptions
           -> YiM a      -- ^ Computation to run
           -> (b -> YiM a) -- ^   Handler
           -> YiM a
catchJustE p (YiM c) h = YiM $ ReaderT (\r -> catchJust p (runReaderT c r) (\b -> runReaderT (runYiM $ h b) r))

handleJustE :: (Exception e) => (e -> Maybe b) -> (b -> YiM a) -> YiM a -> YiM a
handleJustE p h c = catchJustE p c h

-- | Shut down all of our threads. Should free buffers etc.
shutdown :: YiM ()
shutdown = do ts <- threads <$> readsRef yiVar
              liftBase $ mapM_ killThread ts

-- -------------------------------------------

class YiAction a x | a -> x where
    makeAction :: Show x => a -> Action

instance YiAction (IO x) x where
    makeAction = YiA . io

instance YiAction (YiM x) x where
    makeAction = YiA

instance YiAction (EditorM x) x where
    makeAction = EditorA

instance YiAction (BufferM x) x where
    makeAction = BufferA

instance YiAction Action () where
    makeAction = id


instance I.PEq Event where
    equiv = (==)

data KeymapSet = KeymapSet
    { topKeymap :: Keymap         -- ^ Content of the top-level loop.
    , insertKeymap :: Keymap      -- ^ For insertion-only modes
    }

makeLensesWithSuffix "A" ''KeymapSet

extractTopKeymap :: KeymapSet -> Keymap
extractTopKeymap kms = forever (topKeymap kms)
    -- Note the use of "forever": this has quite subtle implications, as it means that
    -- failures in one iteration can yield to jump to the next iteration seamlessly.
    -- eg. in emacs keybinding, failures in incremental search, like <left>, will "exit"
    -- incremental search and immediately move to the left.

modelessKeymapSet :: Keymap -> KeymapSet
modelessKeymapSet k = KeymapSet
 { insertKeymap = k
 , topKeymap = k
 }

-- | @withModeY f@ runs @f@ on the current buffer's mode. As this runs in
-- the YiM monad, we're able to do more than with just 'withModeB' such as
-- prompt the user for something before running the action.
withModeY :: (forall syntax. Mode syntax -> YiM ()) -> YiM ()
withModeY f = do
   bufref <- gets Editor.currentBuffer
   mfbuf <- withEditor $ Editor.findBuffer bufref
   case mfbuf of
     Nothing -> return ()
     Just (FBuffer {bmode = m}) -> f m
