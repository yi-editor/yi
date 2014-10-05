{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Types
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is the host of the most prevalent types throughout Yi.
-- It is unfortunately a necessary evil to avoid use of bootfiles.
--
-- You're encouraged to import from more idiomatic modules which will
-- re-export these where appropriate.

module Yi.Types where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Base
import           Control.Monad.RWS.Strict (RWS, MonadWriter)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Binary (Binary)
import qualified Data.Binary as B
import           Data.Default
import qualified Data.DelayList as DelayList
import           Data.Foldable
import           Data.Function (on)
import           Data.List.NonEmpty
import           Data.List.PointedList
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Time
import           Data.Traversable
import           Data.Typeable
import           Data.Word
#ifdef FRONTEND_VTY
import qualified Graphics.Vty as Vty
#endif
import           Yi.Buffer.Basic (BufferRef, WindowRef)
import           Yi.Buffer.Implementation
import           Yi.Buffer.Undo
import           Yi.Config.Misc
import           Yi.Dynamic
import           Yi.Event
import qualified Yi.Interact as I
import           Yi.KillRing
import           Yi.Layout
import           Yi.Monad
import           Yi.Process (SubprocessInfo, SubprocessId)
import qualified Yi.Rope as R
import           Yi.Style
import           Yi.Style.Library
import           Yi.Syntax
import           Yi.Tab
import           Yi.UI.Common
import           Yi.Window


-- Yi.Keymap

-- TODO: refactor this!

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
            deriving Typeable

emptyAction :: Action
emptyAction = BufferA (return ())

instance Eq Action where
    _ == _ = False

instance Show Action where
    show (YiA _) = "@Y"
    show (EditorA _) = "@E"
    show (BufferA _) = "@B"

type Interact ev a = I.I ev Action a

type KeymapM a = Interact Event a

type Keymap = KeymapM ()

type KeymapEndo = Keymap -> Keymap

type KeymapProcess = I.P Event Action

data IsRefreshNeeded = MustRefresh | NoNeedToRefresh
    deriving (Show, Eq)

data Yi = Yi { yiUi          :: UI Editor
             , yiInput       :: [Event] -> IO ()    -- ^ input stream
             , yiOutput      :: IsRefreshNeeded -> [Action] -> IO ()   -- ^ output stream
             , yiConfig      :: Config
               -- TODO: this leads to anti-patterns and seems like one itself
               -- too coarse for actual concurrency, otherwise pointless
               -- And MVars can be empty so this causes soundness problems
               -- Also makes code a bit opaque
             , yiVar         :: MVar YiVar           -- ^ The only mutable state in the program
             }
             deriving Typeable

data YiVar = YiVar { yiEditor             :: !Editor
                   , yiSubprocessIdSupply :: !SubprocessId
                   , yiSubprocesses       :: !(M.Map SubprocessId SubprocessInfo)
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


data KeymapSet = KeymapSet
    { topKeymap :: Keymap         -- ^ Content of the top-level loop.
    , insertKeymap :: Keymap      -- ^ For insertion-only modes
    }

extractTopKeymap :: KeymapSet -> Keymap
extractTopKeymap kms = forever (topKeymap kms)
    -- Note the use of "forever": this has quite subtle implications, as it means that
    -- failures in one iteration can yield to jump to the next iteration seamlessly.
    -- eg. in emacs keybinding, failures in incremental search, like <left>, will "exit"
    -- incremental search and immediately move to the left.

-- Yi.Buffer.Misc

-- | The BufferM monad writes the updates performed.
newtype BufferM a = BufferM { fromBufferM :: RWS Window [Update] FBuffer a }
    deriving (Monad, Functor, MonadWriter [Update], MonadState FBuffer, MonadReader Window, Typeable)

-- | Currently duplicates some of Vim's indent settings. Allowing a
-- buffer to specify settings that are more dynamic, perhaps via
-- closures, could be useful.
data IndentSettings = IndentSettings
  { expandTabs :: Bool -- ^ Insert spaces instead of tabs as possible
  , tabSize    :: Int  -- ^ Size of a Tab
  , shiftWidth :: Int  -- ^ Indent by so many columns
  } deriving (Eq, Show, Typeable)

instance Applicative BufferM where
    pure = return
    (<*>) = ap

data FBuffer = forall syntax.
        FBuffer { bmode  :: !(Mode syntax)
                , rawbuf :: !(BufferImpl syntax)
                , attributes :: !Yi.Types.Attributes
               }
        deriving Typeable

instance Eq FBuffer where
    (==) = (==) `on` bkey__ . attributes

type WinMarks = MarkSet Mark

data MarkSet a = MarkSet { fromMark, insMark, selMark :: !a }
               deriving (Traversable, Foldable, Functor)

instance Binary a => Binary (MarkSet a) where
  put (MarkSet f i s) = B.put f >> B.put i >> B.put s
  get = liftM3 MarkSet B.get B.get B.get

data Attributes = Attributes
                { ident :: !BufferId
                , bkey__   :: !BufferRef          -- ^ immutable unique key
                , undos  :: !URList               -- ^ undo/redo list
                , bufferDynamic :: !DynamicValues -- ^ dynamic components
                , preferCol :: !(Maybe Int)       -- ^ prefered column to arrive at when we do a lineDown / lineUp
                , pendingUpdates :: ![UIUpdate]   -- ^ updates that haven't been synched in the UI yet
                , selectionStyle :: !SelectionStyle
                , keymapProcess :: !KeymapProcess
                , winMarks :: !(M.Map WindowRef WinMarks)
                , lastActiveWindow :: !Window
                , lastSyncTime :: !UTCTime        -- ^ time of the last synchronization with disk
                , readOnly :: !Bool               -- ^ read-only flag
                , inserting                 :: !Bool -- ^ the keymap is ready for insertion into this buffer
                , directoryContent          :: !Bool -- ^ does buffer contain directory contents
                , pointFollowsWindow        :: !(WindowRef -> Bool)
                , updateTransactionInFlight :: !Bool
                , updateTransactionAccum    :: ![Update]
                } deriving Typeable


instance Binary Yi.Types.Attributes where
    put (Yi.Types.Attributes n b u bd pc pu selectionStyle_
         _proc wm law lst ro ins _dc _pfw isTransacPresent transacAccum) = do
      let putTime (UTCTime x y) = B.put (fromEnum x) >> B.put (fromEnum y)
      B.put n >> B.put b >> B.put u >> B.put bd
      B.put pc >> B.put pu >> B.put selectionStyle_ >> B.put wm
      B.put law >> putTime lst >> B.put ro >> B.put ins >> B.put _dc
      B.put isTransacPresent >> B.put transacAccum
    get = Yi.Types.Attributes <$> B.get <*> B.get <*> B.get <*>
          B.get <*> B.get <*> B.get <*> B.get <*> pure I.End <*> B.get <*> B.get
          <*> getTime <*> B.get <*> B.get <*> B.get
          <*> pure (const False) <*> B.get <*> B.get
      where
        getTime = UTCTime <$> (toEnum <$> B.get) <*> (toEnum <$> B.get)

data BufferId = MemBuffer T.Text
              | FileBuffer FilePath
              deriving (Show, Eq)

instance Binary BufferId where
  get = B.get >>= \case
    (0 :: Word8) -> MemBuffer . E.decodeUtf8 <$> B.get
    1 -> FileBuffer <$> B.get
    x -> fail $ "Binary failed on BufferId, tag: " ++ show x
  put (MemBuffer t) = B.put (0 :: Word8) >> B.put (E.encodeUtf8 t)
  put (FileBuffer t) = B.put (1 :: Word8) >> B.put t


data SelectionStyle = SelectionStyle
  { highlightSelection :: !Bool
  , rectangleSelection :: !Bool
  } deriving Typeable

instance Binary SelectionStyle where
  put (SelectionStyle h r) = B.put h >> B.put r
  get = SelectionStyle <$> B.get <*> B.get


data AnyMode = forall syntax. AnyMode (Mode syntax)
  deriving Typeable

-- | A Mode customizes the Yi interface for editing a particular data
-- format. It specifies when the mode should be used and controls
-- file-specific syntax highlighting and command input, among other
-- things.
data Mode syntax = Mode
  { modeName :: T.Text
    -- ^ so this can be serialized, debugged.
  , modeApplies :: FilePath -> R.YiString -> Bool
    -- ^ What type of files does this mode apply to?
  , modeHL :: ExtHL syntax
    -- ^ Syntax highlighter
  , modePrettify :: syntax -> BufferM ()
    -- ^ Prettify current \"paragraph\"
  , modeKeymap :: KeymapSet -> KeymapSet
    -- ^ Buffer-local keymap modification
  , modeIndent :: syntax -> IndentBehaviour -> BufferM ()
    -- ^ emacs-style auto-indent line
  , modeAdjustBlock :: syntax -> Int -> BufferM ()
    -- ^ adjust the indentation after modification
  , modeFollow :: syntax -> Action
    -- ^ Follow a \"link\" in the file. (eg. go to location of error message)
  , modeIndentSettings :: IndentSettings
  , modeToggleCommentSelection :: Maybe (BufferM ())
  , modeGetStrokes :: syntax -> Point -> Point -> Point -> [Stroke]
    -- ^ Strokes that should be applied when displaying a syntax element
    -- should this be an Action instead?
  , modeOnLoad :: BufferM ()
    -- ^ An action that is to be executed when this mode is set
  , modeModeLine :: [T.Text] -> BufferM T.Text
    -- ^ buffer-local modeline formatting method
  }

-- | Used to specify the behaviour of the automatic indent command.
data IndentBehaviour =
    IncreaseCycle -- ^ Increase the indentation to the next higher indentation
                  --   hint. If we are currently at the highest level of
                  --   indentation then cycle back to the lowest.
  | DecreaseCycle -- ^ Decrease the indentation to the next smaller indentation
                  --   hint. If we are currently at the smallest level then
                  --   cycle back to the largest
  | IncreaseOnly  -- ^ Increase the indentation to the next higher hint
                  --   if no such hint exists do nothing.
  | DecreaseOnly  -- ^ Decrease the indentation to the next smaller indentation
                  --   hint, if no such hint exists do nothing.
    deriving (Eq, Show)

-- Yi.Editor

type Status = ([T.Text], StyleName)
type Statuses = DelayList.DelayList Status

-- | The Editor state
data Editor = Editor
  { bufferStack     :: !(NonEmpty BufferRef)
    -- ^ Stack of all the buffers.
    -- Invariant: first buffer is the current one.
  , buffers         :: !(M.Map BufferRef FBuffer)
  , refSupply       :: !Int  -- ^ Supply for buffer, window and tab ids.
  , tabs_           :: !(PointedList Tab)
    -- ^ current tab contains the visible windows pointed list.
  , dynamic         :: !DynamicValues -- ^ dynamic components
  , statusLines     :: !Statuses
  , maxStatusHeight :: !Int
  , killring        :: !Killring
  , currentRegex    :: !(Maybe SearchExp)
    -- ^ currently highlighted regex (also most recent regex for use
    -- in vim bindings)
  , searchDirection :: !Direction
  , pendingEvents   :: ![Event]
    -- ^ Processed events that didn't yield any action yet.
  , onCloseActions  :: !(M.Map BufferRef (EditorM ()))
    -- ^ Actions to be run when the buffer is closed; should be scrapped.
  } deriving Typeable


newtype EditorM a = EditorM {fromEditorM :: ReaderT Config (State Editor) a}
    deriving (Monad, Applicative, MonadState Editor,
              MonadReader Config, Functor)

instance MonadEditor EditorM where
    askCfg = ask
    withEditor = id

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 EditorM
#else
deriving instance Typeable EditorM
#endif

class (Monad m, MonadState Editor m) => MonadEditor m where
  askCfg :: m Config

  withEditor :: EditorM a -> m a
  withEditor f = do
    cfg <- askCfg
    getsAndModify (runEditor cfg f)

  withEditor_ :: EditorM a -> m ()
  withEditor_ = withEditor . void


runEditor :: Config -> EditorM a -> Editor -> (Editor, a)
runEditor cfg f e = let (a, e') = runState (runReaderT (fromEditorM f) cfg) e
                    in (e',a)

-- Yi.Config

data UIConfig = UIConfig {
#ifdef FRONTEND_VTY
   configVty :: Vty.Config,
#endif
   configFontName :: Maybe String,  -- ^ Font name, for the UI that support it.
   configFontSize :: Maybe Int,     -- ^ Font size, for the UI that support it.
   configScrollStyle :: Maybe ScrollStyle,
   -- ^ Style of scroll
   configScrollWheelAmount :: Int,  -- ^ Amount to move the buffer when using the scroll wheel
   configLeftSideScrollBar :: Bool, -- ^ Should the scrollbar be shown on the left side?
   configAutoHideScrollBar :: Bool, -- ^ Hide scrollbar automatically if text fits on one page.
   configAutoHideTabBar :: Bool,    -- ^ Hide the tabbar automatically if only one tab is present
   configLineWrap :: Bool,          -- ^ Wrap lines at the edge of the window if too long to display.
   configCursorStyle :: CursorStyle,
   configWindowFill :: Char,
   -- ^ The char with which to fill empty window space.  Usually '~' for vi-like
   -- editors, ' ' for everything else.
   configTheme :: Theme             -- ^ UI colours
  }


type UIBoot = Config -> ([Event] -> IO ())
              -> ([Action] -> IO ()) ->  Editor -> IO (UI Editor)

-- | When should we use a "fat" cursor (i.e. 2 pixels wide, rather than 1)? Fat
-- cursors have only been implemented for the Pango frontend.
data CursorStyle = AlwaysFat
                 | NeverFat
                 | FatWhenFocused
                 | FatWhenFocusedAndInserting

-- | Configuration record. All Yi hooks can be set here.
data Config = Config {startFrontEnd :: UIBoot,
                      -- ^ UI to use.
                      configUI :: UIConfig,
                      -- ^ UI-specific configuration.
                      startActions :: [Action],
                      -- ^ Actions to run when the editor is started.
                      initialActions :: [Action],
                      -- ^ Actions to run after startup (after startActions) or reload.
                      defaultKm :: KeymapSet,
                      -- ^ Default keymap to use.
                      configInputPreprocess :: I.P Event Event,
                      modeTable :: [AnyMode],
                      -- ^ List modes by order of preference.
                      debugMode :: Bool,
                      -- ^ Produce a .yi.dbg file with a lot of debug information.
                      configRegionStyle :: RegionStyle,
                      -- ^ Set to 'Exclusive' for an emacs-like behaviour.
                      configKillringAccumulate :: Bool,
                      -- ^ Set to 'True' for an emacs-like behaviour, where
                      -- all deleted text is accumulated in a killring.
                      configCheckExternalChangesObsessively :: Bool,
                      bufferUpdateHandler :: [[Update] -> BufferM ()],
                      layoutManagers :: [AnyLayoutManager],
                      -- ^ List of layout managers for 'cycleLayoutManagersNext'
                      configVars :: ConfigVariables
                      -- ^ Custom configuration, containing the 'YiConfigVariable's. Configure with 'configVariableA'.
                     }


-- Yi.Buffer.Normal

-- Region styles are relative to the buffer contents.
-- They likely should be considered a TextUnit.
data RegionStyle = LineWise
                 | Inclusive
                 | Exclusive
                 | Block
  deriving (Eq, Typeable, Show)

instance Binary RegionStyle where
  put LineWise = B.put (0 :: Word8)
  put Inclusive = B.put (1 :: Word8)
  put Exclusive = B.put (2 :: Word8)
  put Block = B.put (3 :: Word8)

  get = B.get >>= \case
    (0 :: Word8) -> return LineWise
    1 -> return Inclusive
    2 -> return Exclusive
    3 -> return Block
    n -> fail $ "Binary RegionStyle fail with " ++ show n

-- TODO: put in the buffer state proper.
instance Default RegionStyle where
  def = Inclusive

instance YiVariable RegionStyle
