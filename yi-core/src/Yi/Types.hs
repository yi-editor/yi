{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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

import           Control.Concurrent             (MVar, modifyMVar, modifyMVar_, readMVar)
import           Control.Monad.Base             (MonadBase, liftBase)
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad (ap, liftM3, void, forever)
import qualified Data.Set                       as Set
import           Data.Binary                    (Binary)
import qualified Data.Binary                    as B (get, put)
import           Data.Default                   (Default, def)
import qualified Data.DelayList                 as DelayList (DelayList)
import qualified Data.DynamicState              as ConfigState (DynamicState)
import qualified Data.DynamicState.Serializable as DynamicState (DynamicState)
import           Data.Function                  (on)
import           Data.List.NonEmpty             (NonEmpty)
import           Data.List.PointedList          (PointedList)
import qualified Data.Map.Strict                as M (Map)
import qualified Data.Text                      as T (Text)
import qualified Data.Text.Encoding             as E (decodeUtf8, encodeUtf8)
import           Data.Time                      (UTCTime (..))
import           Data.Typeable                  (Typeable)
import qualified Data.Sequence                  as S
import           Data.Word                      (Word8)
import           Yi.Buffer.Basic                (BufferRef, WindowRef)
import           Yi.Buffer.Implementation
import           Yi.Buffer.Undo                 (URList)
import           Yi.Config.Misc                 (ScrollStyle)
import           Yi.Event                       (Event)
import qualified Yi.Interact                    as I (I, P (End))
import           Yi.KillRing                    (Killring)
import           Yi.Layout                      (AnyLayoutManager)
import           Yi.Monad                       (getsAndModify)
import           Yi.Process                     (SubprocessId, SubprocessInfo)
import qualified Yi.Rope                        as R (YiString)
import           Yi.Style                       (StyleName)
import           Yi.Style.Library               (Theme)
import           Yi.Syntax                      (ExtHL, Stroke)
import           Yi.Tab                         (Tab)
import           Yi.UI.Common                   (UI)
import           Yi.Window                      (Window)


-- Yi.Keymap

-- TODO: refactor this!

data Action = forall a. Show a => YiA (YiM a)
            | forall a. Show a => EditorA (EditorM a)
            | forall a. Show a => BufferA (BufferM a)
            deriving Typeable

emptyAction :: Action
emptyAction = BufferA (return ())

class (Default a, Binary a, Typeable a) => YiVariable a
class (Default a, Typeable a) => YiConfigVariable a

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
    get = yiEditor <$> (liftBase . readMVar =<< yiVar <$> ask)
    put v = liftBase . flip modifyMVar_ (\x -> return $ x {yiEditor = v}) =<< yiVar <$> ask

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
newtype BufferM a = BufferM { fromBufferM :: ReaderT Window (State FBuffer) a }
    deriving ( Monad, Functor, Typeable
             , MonadState FBuffer
             , MonadReader Window )

-- | Currently duplicates some of Vim's indent settings. Allowing a
-- buffer to specify settings that are more dynamic, perhaps via
-- closures, could be useful.
data IndentSettings = IndentSettings
  { expandTabs :: !Bool -- ^ Insert spaces instead of tabs as possible
  , tabSize    :: !Int  -- ^ Size of a Tab
  , shiftWidth :: !Int  -- ^ Indent by so many columns
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
               deriving (Traversable, Foldable, Functor, Show)

instance Binary a => Binary (MarkSet a) where
  put (MarkSet f i s) = B.put f >> B.put i >> B.put s
  get = liftM3 MarkSet B.get B.get B.get

data Attributes
    = Attributes
    { ident :: !BufferId
    , bkey__   :: !BufferRef -- ^ immutable unique key
    , undos  :: !URList -- ^ undo/redo list
    , bufferDynamic :: !DynamicState.DynamicState -- ^ dynamic components
    , preferCol :: !(Maybe Int)
    -- ^ prefered column to arrive at when we do a lineDown / lineUp
    , preferVisCol :: !(Maybe Int)
    -- ^ prefered column to arrive at visually (ie, respecting wrap)
    , stickyEol :: !Bool
    -- ^ stick to the end of line (used by vim bindings mostly)
    , pendingUpdates :: !(S.Seq UIUpdate)
    -- ^ updates that haven't been synched in the UI yet
    , selectionStyle :: !SelectionStyle
    , keymapProcess :: !KeymapProcess
    , winMarks :: !(M.Map WindowRef WinMarks)
    , lastActiveWindow :: !Window
    , lastSyncTime :: !UTCTime
    -- ^ time of the last synchronization with disk
    , readOnly :: !Bool -- ^ read-only flag
    , inserting :: !Bool -- ^ the keymap is ready for insertion into this buffer
    , directoryContent :: !Bool -- ^ does buffer contain directory contents
    , pointFollowsWindow :: !(Set.Set WindowRef)
    , updateTransactionInFlight :: !Bool
    , updateTransactionAccum :: !(S.Seq Update)
    , fontsizeVariation :: !Int
      -- ^ How many points (frontend-specific) to change
      -- the font by in this buffer
    , updateStream :: !(S.Seq Update)
    -- ^ Updates that we've seen in this buffer, basically
    -- "write-only". Work-around for broken WriterT.
    } deriving Typeable


instance Binary Yi.Types.Attributes where
    put (Yi.Types.Attributes n b u bd pc pv se pu selectionStyle_
         _proc wm law lst ro ins _dc _pfw isTransacPresent transacAccum fv lg') = do
      let putTime (UTCTime x y) = B.put (fromEnum x) >> B.put (fromEnum y)
      B.put n >> B.put b >> B.put u >> B.put bd
      B.put pc >> B.put pv >> B.put se >> B.put pu >> B.put selectionStyle_ >> B.put wm
      B.put law >> putTime lst >> B.put ro >> B.put ins >> B.put _dc
      B.put isTransacPresent >> B.put transacAccum >> B.put fv >> B.put lg'
    get = Yi.Types.Attributes <$> B.get <*> B.get <*> B.get <*> B.get <*>
          B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> pure I.End <*> B.get <*> B.get
          <*> getTime <*> B.get <*> B.get <*> B.get
          <*> pure ({- TODO can serialise now -}mempty) <*> B.get <*> B.get <*> B.get <*> B.get
      where
        getTime = UTCTime <$> (toEnum <$> B.get) <*> (toEnum <$> B.get)

data BufferId = MemBuffer !T.Text
              | FileBuffer !FilePath
              deriving (Show, Eq, Ord)

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
  } deriving (Typeable, Show)

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
  , modeGotoDeclaration :: BufferM ()
    -- ^ go to the point where the variable is declared
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
  , dynamic         :: !DynamicState.DynamicState -- ^ dynamic components
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
              MonadReader Config, Functor, Typeable)

instance MonadEditor EditorM where
    askCfg = ask
    withEditor = id

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
   configTheme :: Theme,            -- ^ UI colours
   configLineNumbers :: Bool        -- ^ Should we show line numbers by default?
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
                      configUI :: !UIConfig,
                      -- ^ UI-specific configuration.
                      startActions :: ![Action],
                      -- ^ Actions to run when the editor is started.
                      initialActions :: ![Action],
                      -- ^ Actions to run after startup (after startActions) or reload.
                      defaultKm :: !KeymapSet,
                      -- ^ Default keymap to use.
                      configInputPreprocess :: !(I.P Event Event),
                      modeTable :: ![AnyMode],
                      -- ^ List modes by order of preference.
                      debugMode :: !Bool,
                      -- ^ Produce a .yi.dbg file with a lot of debug information.
                      configRegionStyle :: !RegionStyle,
                      -- ^ Set to 'Exclusive' for an emacs-like behaviour.
                      configKillringAccumulate :: !Bool,
                      -- ^ Set to 'True' for an emacs-like behaviour, where
                      -- all deleted text is accumulated in a killring.
                      configCheckExternalChangesObsessively :: !Bool,
                      bufferUpdateHandler :: !(S.Seq (S.Seq Update -> BufferM ())),
                      layoutManagers :: ![AnyLayoutManager],
                      -- ^ List of layout managers for 'cycleLayoutManagersNext'
                      configVars :: !ConfigState.DynamicState,
                      -- ^ Custom configuration, containing the 'YiConfigVariable's. Configure with 'configVariableA'.
                      configDisableSystemClipboard :: !Bool
                      -- ^ Set to 'True' not to use system clipboard.
                      -- When vty-mode, system clipboard is not available in some environments.
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
