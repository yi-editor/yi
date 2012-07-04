{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ExistentialQuantification, MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving, Rank2Types, TemplateHaskell #-}

-- Copyright (c) Jean-Philippe Bernardy 2007,8.

module Yi.Keymap where

import Control.Applicative 
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Data.Typeable
import Prelude hiding (error, catch)
import Yi.Buffer
import Yi.Config
import Yi.Editor (EditorM, Editor, runEditor, MonadEditor(..))
import Yi.Event
import Yi.Monad
import Yi.Prelude (io)
import Yi.Process (SubprocessInfo, SubprocessId)
import Yi.UI.Common
import qualified Data.Map as M
import qualified Yi.Editor as Editor
import qualified Yi.Interact as I
import Data.Accessor.Template

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
            | TaggedA String Action
--            | InsertA String
--             | TextA Direction Unit Operation
        deriving Typeable

emptyAction :: Action
emptyAction = BufferA (return ())

instance I.PEq Action where
    equiv _ _ = False

instance Show Action where
    show (YiA _) = "@Y"
    show (EditorA _) = "@E"
    show (BufferA _) = "@B"
    show (TaggedA s a) = s ++ show a

type Interact ev a = I.I ev Action a

type KeymapM a = Interact Event a

type Keymap = KeymapM ()

type KeymapEndo = Keymap -> Keymap

type KeymapProcess = I.P Event Action

data Yi = Yi {yiUi          :: UI,
              input         :: Event -> IO (),      -- ^ input stream
              output        :: [Action] -> IO (),   -- ^ output stream
              yiConfig      :: Config,
              yiVar         :: MVar YiVar           -- ^ The only mutable state in the program
             }
             deriving Typeable

data YiVar = YiVar {yiEditor             :: !Editor,
                    threads              :: ![ThreadId],           -- ^ all our threads
                    yiSubprocessIdSupply :: !SubprocessId,
                    yiSubprocesses       :: !(M.Map SubprocessId SubprocessInfo)
                   }

-- | The type of user-bindable functions
newtype YiM a = YiM {runYiM :: ReaderT Yi IO a}
    deriving (Monad, MonadReader Yi, MonadIO, Typeable, Functor)

instance MonadState Editor YiM where
    get = yiEditor <$> (readRef =<< yiVar <$> ask)
    put v = flip modifyRef (\x -> x {yiEditor = v}) =<< yiVar <$> ask

instance MonadEditor YiM where
    askCfg = yiConfig <$> ask
    withEditor f = do
      r <- asks yiVar
      cfg <- asks yiConfig
      liftIO $ unsafeWithEditor cfg r f
 
-----------------------
-- Keymap basics
 
-- | @write a@ returns a keymap that just outputs the action @a@.
write :: (I.MonadInteract m Action ev, YiAction a x, Show x) => a -> m ()
write x = I.write (makeAction x)

write' :: (I.MonadInteract m Action e, YiAction a x, Show x) => String -> a -> m ()
write' s x = I.write (TaggedA s (makeAction x))

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

withGivenBuffer :: BufferRef -> BufferM a -> YiM a
withGivenBuffer b f = withEditor (Editor.withGivenBuffer0 b f)

withBuffer :: BufferM a -> YiM a
withBuffer f = withEditor (Editor.withBuffer0 f)

readEditor :: (Editor -> a) -> YiM a
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
              liftIO $ mapM_ killThread ts

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
    , startInsertKeymap :: Keymap -- ^ Startup when entering insert mode
    , insertKeymap :: Keymap      -- ^ For insertion-only modes 
    , startTopKeymap :: Keymap    -- ^ Startup bit, to execute only once at the beginning.
    }

$(nameDeriveAccessors ''KeymapSet $ Just.(++ "A"))

extractTopKeymap :: KeymapSet -> Keymap
extractTopKeymap kms = do
    startTopKeymap kms >> forever (topKeymap kms)
    -- Note the use of "forever": this has quite subtle implications, as it means that
    -- failures in one iteration can yield to jump to the next iteration seamlessly.
    -- eg. in emacs keybinding, failures in incremental search, like <left>, will "exit"
    -- incremental search and immediately move to the left.

modelessKeymapSet :: Keymap -> KeymapSet
modelessKeymapSet k = KeymapSet
 { insertKeymap = k
 , startInsertKeymap = return ()
 , topKeymap = k
 , startTopKeymap = return ()
 }
