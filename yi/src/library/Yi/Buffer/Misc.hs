{-# LANGUAGE
  TemplateHaskell,
  CPP,
  GeneralizedNewtypeDeriving,
  DeriveDataTypeable,
  DeriveTraversable,
  DeriveFoldable,
  DeriveFunctor,
  StandaloneDeriving,
  ExistentialQuantification,
  Rank2Types,
  ImpredicativeTypes,
  TypeSynonymInstances,
  FlexibleContexts,
  StandaloneDeriving,
  DeriveGeneric #-}

-- Copyright (C) 2004, 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (C) 2007, 2008 JP Bernardy

-- | The 'Buffer' module defines monadic editing operations over one-dimensional
-- buffers, maintaining a current /point/.

module Yi.Buffer.Misc
  ( FBuffer (FBuffer, bmode)
  , BufferM (..)
  , WinMarks, MarkSet (..)
  , bkey
  , getMarks
  , runBuffer
  , runBufferFull
  , runBufferDummyWindow
  , curLn
  , curCol
  , colOf
  , lineOf
  , lineCountB
  , sizeB
  , pointB
  , pointOfLineColB
  , solPointB
  , markLines
  , moveTo
  , moveToColB
  , moveToLineColB
  , lineMoveRel
  , lineUp
  , lineDown
  , newB
  , MarkValue(..)
  , Overlay, OvlLayer(..)
  , mkOverlay
  , gotoLn
  , gotoLnFrom
  , leftB
  , rightB
  , moveN
  , leftN
  , rightN
  , insertN'
  , insertN
  , insertNAt'
  , insertNAt
  , insertB
  , deleteN
  , nelemsB
  , nelemsB'
  , writeB
  , writeN
  , newlineB
  , deleteNAt
  , readB
  , elemsB
  , undosA
  , undoB
  , redoB
  , getMarkB
  , setMarkHereB
  , setNamedMarkHereB
  , mayGetMarkB
  , getMarkValueB
  , markPointA
  , modifyMarkB
  , newMarkB
  , deleteMarkB
  , setVisibleSelection
  , isUnchangedBuffer
  , setAnyMode
  , setMode
  , setMode0
  , modifyMode
  , regexRegionB
  , regexB
  , readAtB
  , getModeLine
  , getPercent
  , setInserting
  , savingPrefCol
  , forgetPreferCol
  , movingToPrefCol
  , preferColA
  , markSavedB
  , addOverlayB
  , delOverlayB
  , delOverlayLayerB
  , savingExcursionB
  , savingPointB
  , pendingUpdatesA
  , highlightSelectionA
  , rectangleSelectionA
  , readOnlyA
  , insertingA
  , pointFollowsWindowA
  , revertPendingUpdatesB
  , askWindow
  , clearSyntax
  , focusSyntax
  , Mode (..)
  , modeNameA
  , modeAppliesA
  , modeHLA
  , modePrettifyA
  , modeKeymapA
  , modeIndentA
  , modeAdjustBlockA
  , modeFollowA
  , modeIndentSettingsA
  , modeToggleCommentSelectionA
  , modeGetStrokesA
  , modeGetAnnotationsA
  , modeOnLoadA
  , modeModeLineA
  , AnyMode (..)
  , IndentBehaviour (..)
  , IndentSettings (..)
  , expandTabsA
  , tabSizeA
  , shiftWidthA
  , modeAlwaysApplies
  , modeNeverApplies
  , emptyMode
  , withModeB
  , withMode0
  , onMode
  , withSyntaxB
  , withSyntaxB'
  , keymapProcessA
  , strokesRangesB
  , streamB
  , indexedStreamB
  , askMarks
  , pointAt
  , SearchExp
  , lastActiveWindowA
  , bufferDynamicValueA
  , shortIdentString
  , identString
  , miniIdentString
  , identA
  , directoryContentA
  , BufferId
  , file
  , lastSyncTimeA
  , replaceCharB
  , replaceCharWithBelowB
  , replaceCharWithAboveB
  , insertCharWithBelowB
  , insertCharWithAboveB
  , pointAfterCursorB
  , destinationOfMoveB
  , withEveryLineB
  , startUpdateTransactionB
  , commitUpdateTransactionB
  )
where

import Prelude hiding (foldr, mapM, notElem)
import Yi.Region
import System.FilePath
import Yi.Buffer.Implementation
import Yi.Syntax
import Yi.Buffer.Undo
import Yi.Dynamic
import Yi.Window
import Control.Monad.RWS.Strict hiding (mapM_, mapM, get, put, forM_, forM)
import Control.Applicative
import Control.Lens hiding ((+~), Action, reversed, at, act)
import Data.Binary
import Data.Default
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.Foldable
import Data.Traversable
import Data.Typeable
import Data.Function hiding ((.), id)
import Data.Rope (Rope)
import qualified Data.Rope as R
import qualified Data.Map as M
import Data.Maybe
import {-# source #-} Yi.Keymap
import Yi.Interact as I
import Yi.Buffer.Basic
import {-# SOURCE #-} Yi.Buffer.HighLevel
import {-# SOURCE #-} Yi.MiniBuffer (withMinibufferFree)
import Yi.Monad
import Yi.Utils
import Data.Time
import Numeric(showHex)
import Data.Char(ord)

#ifdef TESTING
-- TODO: make this compile.

-- import Test.QuickCheck
-- import Driver ()

-- instance Arbitrary FBuffer where
--     arbitrary = do b0 <- return (newB 0 "*buffername*") `ap` (LazyUTF8.fromString `fmap` arbitrary)
--                    p0 <- arbitrary
--                    return $ snd $ runBuffer (dummyWindow $ bkey b0) b0 (moveTo $ Point p0)

-- prop_replace_point b = snd $ runBufferDummyWindow b $ do
--   p0 <- pointB
--   replaceRegionB r
--   p1 <- pointB
--   return $ (p1 - p0) == ...
#endif

-- In addition to Buffer's text, this manages (among others):
--  * Log of updates mades
--  * Undo

type WinMarks = MarkSet Mark

data MarkSet a = MarkSet { fromMark, insMark, selMark :: !a } deriving (Traversable, Foldable, Functor)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''MarkSet)
#else
deriving instance Generic (MarkSet a)
instance Binary a => Binary (MarkSet a)
#endif

data SelectionStyle = SelectionStyle
  { highlightSelection :: !Bool
  , rectangleSelection :: !Bool
  }
  deriving Typeable

type BufferId = Either String FilePath
-- ^ maybe a filename associated with this buffer. Filename is canonicalized.

-- TODO:
-- data BufferIdent
--     = MemBuffer String -- ^ Buffer ident
--     | FileBuffer FilePath

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

makeClassyWithSuffix "A" ''Attributes

instance Binary Attributes where
    put (Attributes n b u bd pc pu selectionStyle_ _proc wm law lst ro ins _dc _pfw
        isTransacPresent transacAccum) = do
          put n >> put b >> put u >> put bd
          put pc >> put pu >> put selectionStyle_ >> put wm
          put law >> put lst >> put ro >> put ins >> put _dc >> put isTransacPresent >> put transacAccum
    get = Attributes <$> get <*> get <*> get <*>
          get <*> get <*> get <*> get <*> pure I.End <*> get <*> get <*> get <*> get <*> get <*>
              get <*> pure (const False) <*> get <*> get

instance Binary UTCTime where
    put (UTCTime x y) = put (fromEnum x) >> put (fromEnum y)
    get = UTCTime <$> (toEnum <$> get) <*> (toEnum <$> get)

data FBuffer = forall syntax.
        FBuffer { bmode  :: !(Mode syntax)
                , rawbuf :: !(BufferImpl syntax)
                , attributes :: !Attributes
               }
        deriving Typeable

instance HasAttributes FBuffer where
    attributesA = lens attributes (\(FBuffer f1 f2 _) a -> FBuffer f1 f2 a)

shortIdentString :: [a] -> FBuffer -> String
shortIdentString prefix b = case b ^. identA of
    Left bName -> "*" ++ bName ++ "*"
    Right fName -> joinPath $ drop (length prefix) $ splitPath fName

identString :: FBuffer -> String
identString b = case b ^. identA of
    Left bName -> "*" ++ bName ++ "*"
    Right fName -> fName

miniIdentString :: FBuffer -> String
miniIdentString b = case b ^. identA of
    Right _ -> "MINIFILE:"
    Left bufName -> bufName

-- unfortunately the dynamic stuff can't be read.
instance Binary FBuffer where
    put (FBuffer binmode r attributes_) =
      let strippedRaw :: BufferImpl ()
          strippedRaw = setSyntaxBI (modeHL emptyMode) r
      in do
          put binmode
          put strippedRaw
          put attributes_
    get = do
        FBuffer <$> get <*> getStripped <*> get
      where getStripped :: Get (BufferImpl ())
            getStripped = get

instance Binary SelectionStyle where
  put (SelectionStyle h r) = put h >> put r
  get = SelectionStyle <$> get <*> get

-- | update the syntax information (clear the dirty "flag")
clearSyntax :: FBuffer -> FBuffer
clearSyntax = modifyRawbuf updateSyntax

queryRawbuf :: (forall syntax. BufferImpl syntax -> x) -> FBuffer -> x
queryRawbuf f (FBuffer _ fb _) = f fb

modifyRawbuf :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> FBuffer -> FBuffer
modifyRawbuf f (FBuffer f1 f2 f3) = FBuffer f1 (f f2) f3

queryAndModifyRawbuf :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) ->
                     FBuffer -> (FBuffer, x)
queryAndModifyRawbuf f (FBuffer f1 f5 f3) =
    let (f5', x) = f f5
    in (FBuffer f1 f5' f3, x)

file :: FBuffer -> Maybe FilePath
file b = case b ^. identA of
    Right f -> Just f
    _ -> Nothing

highlightSelectionA :: Lens' FBuffer Bool
highlightSelectionA = selectionStyleA .
  lens highlightSelection (\e x -> e { highlightSelection = x })

rectangleSelectionA :: Lens' FBuffer Bool
rectangleSelectionA = selectionStyleA .
  lens rectangleSelection (\e x -> e { rectangleSelection = x })

{- | Currently duplicates some of Vim's indent settings. Allowing a buffer to
 - specify settings that are more dynamic, perhaps via closures, could be
 - useful.
 -}
data IndentSettings = IndentSettings { expandTabs :: Bool -- ^ Insert spaces instead of tabs as possible
                                     , tabSize    :: Int  -- ^ Size of a Tab
                                     , shiftWidth :: Int  -- ^ Indent by so many columns
                                     }
                      deriving (Eq, Show, Typeable)



data AnyMode = forall syntax. AnyMode (Mode syntax)
  deriving Typeable

{- | A Mode customizes the Yi interface for editing a particular data
   format.  It specifies when the mode should be used and
   controls file-specific syntax highlighting and command input, among
   other things.
-}
data Mode syntax = Mode
    {
     modeName :: String,              -- ^ so this can be serialized, debugged.
     modeApplies :: FilePath -> String -> Bool, -- ^ What type of files does this mode apply to?
     modeHL :: ExtHL syntax,          -- ^ Syntax highlighter
     modePrettify :: syntax -> BufferM (), -- ^ Prettify current \"paragraph\"
     modeKeymap :: KeymapSet -> KeymapSet, -- ^ Buffer-local keymap modification
     modeIndent :: syntax -> IndentBehaviour -> BufferM (), -- ^ emacs-style auto-indent line
     modeAdjustBlock :: syntax -> Int -> BufferM (), -- ^ adjust the indentation after modification
     modeFollow :: syntax -> Action, -- ^ Follow a \"link\" in the file. (eg. go to location of error message)
     modeIndentSettings :: IndentSettings,
     modeToggleCommentSelection :: YiM (),
     modeGetStrokes :: syntax -> Point -> Point -> Point -> [Stroke], -- ^ Strokes that should be applied when displaying a syntax element
     modeGetAnnotations :: syntax -> Point -> [Span String],
     -- should this be an Action instead?
     modeOnLoad :: BufferM (), -- ^ An action that is to be executed when this mode is set
     modeModeLine :: [String] -> BufferM String -- ^ buffer-local modeline formatting method
    }

instance Binary (Mode syntax) where
    put = put . modeName -- we just store the modename.
    get = do n <- get
             return (emptyMode {modeName = n})

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


-- | The BufferM monad writes the updates performed.
newtype BufferM a = BufferM { fromBufferM :: RWS Window [Update] FBuffer a }
    deriving (Monad, Functor, MonadWriter [Update], MonadState FBuffer, MonadReader Window, Typeable)

-- deriving instance Typeable4 RWS

instance Applicative BufferM where
    pure = return
    (<*>) = ap

instance Eq FBuffer where
    (==) = (==) `on` bkey

instance Show FBuffer where
    show b = "Buffer #" ++ show (bkey b) ++ " (" ++ identString b ++ ")"

-- | Given a buffer, and some information update the modeline
--
-- N.B. the contents of modelines should be specified by user, and
-- not hardcoded.
--

getModeLine :: [String] -> BufferM String
getModeLine prefix = withModeB (`modeModeLine` prefix)

defaultModeLine :: [String] -> BufferM String
defaultModeLine prefix = do
    col <- curCol
    pos <- pointB
    ln <- curLn
    p <- pointB
    s <- sizeB
    curChar <-readB
    ro <-use readOnlyA
    modeNm <- gets (withMode0 modeName)
    unchanged <- gets isUnchangedBuffer
    let pct
          | pos == 0 || s == 0 = " Top"
          | pos == s = " Bot"
          | otherwise = getPercent p s
        chg = if unchanged then "-" else "*"
        roStr = if ro  then "%" else chg
        hexChar = "0x" ++ padString 2 '0' (Numeric.showHex (Data.Char.ord curChar) "")

    nm <- gets $ shortIdentString prefix
    return $
           roStr ++ chg ++ " "
           ++ nm ++
           replicate 5 ' ' ++
           hexChar ++ "  " ++
           "L" ++ padString 5 ' ' (show ln) ++ "  " ++ "C" ++ padString 3 ' ' (show col) ++
           "  " ++ pct ++
           "  " ++ modeNm ++
           "  " ++ show (fromPoint p)

padString :: Int -> Char -> String -> String
padString n c s = replicate k c ++ s
  where
    k = max 0 $ n - length s

-- | Given a point, and the file size, gives us a percent string
getPercent :: Point -> Point -> String
getPercent a b = padString 3 ' ' (show p) ++ "%"
    where p = ceiling (aa / bb * 100.0 :: Double) :: Int
          aa = fromIntegral a :: Double
          bb = fromIntegral b :: Double

queryBuffer :: (forall syntax. BufferImpl syntax -> x) -> BufferM x
queryBuffer = gets . queryRawbuf

modifyBuffer :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> BufferM ()
modifyBuffer = modify . modifyRawbuf

queryAndModify :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) -> BufferM x
queryAndModify = getsAndModify . queryAndModifyRawbuf

-- | Adds an "overlay" to the buffer
addOverlayB :: Overlay -> BufferM ()
addOverlayB ov = do
  (%=) pendingUpdatesA (++ [overlayUpdate ov])
  modifyBuffer $ addOverlayBI ov

-- | Remove an existing "overlay"
delOverlayB :: Overlay -> BufferM ()
delOverlayB ov = do
  (%=) pendingUpdatesA (++ [overlayUpdate ov])
  modifyBuffer $ delOverlayBI ov

delOverlayLayerB :: OvlLayer -> BufferM ()
delOverlayLayerB l =
  modifyBuffer $ delOverlayLayer l

-- | Execute a @BufferM@ value on a given buffer and window.  The new state of
-- the buffer is returned alongside the result of the computation.
runBuffer :: Window -> FBuffer -> BufferM a -> (a, FBuffer)
runBuffer w b f =
    let (a, _, b') = runBufferFull w b f
    in (a, b')

getMarks :: Window -> BufferM (Maybe WinMarks)
getMarks = gets . getMarksRaw

getMarksRaw :: Window -> FBuffer -> Maybe WinMarks
getMarksRaw w b = M.lookup (wkey w) (b ^. winMarksA)

runBufferFull :: Window -> FBuffer -> BufferM a -> (a, [Update], FBuffer)
runBufferFull w b f =
    let (a, b', updates) = runRWS (fromBufferM f') w b
        f' = do
            ms <- getMarks w
            when (isNothing ms) $ do
                -- this window has no marks for this buffer yet; have to create them.
                newMarkValues <- if wkey (b ^. lastActiveWindowA) == def
                    then return
                        -- no previous window, create some marks from scratch.
                         MarkSet { insMark = MarkValue 0 Forward,
                                   selMark = MarkValue 0 Backward, -- sel
                                   fromMark = MarkValue 0 Backward } -- from
                    else do
                        Just mrks  <- uses winMarksA (M.lookup $ wkey (b ^. lastActiveWindowA))
                        forM mrks getMarkValueB
                newMrks <- forM newMarkValues newMarkB
                (%=) winMarksA (M.insert (wkey w) newMrks)
            assign lastActiveWindowA w
            f
    in (a, updates, pendingUpdatesA %~ (++ fmap TextUpdate updates) $ b')

getMarkValueRaw :: Mark -> FBuffer -> MarkValue
getMarkValueRaw m = fromMaybe (MarkValue 0 Forward) . queryRawbuf (getMarkValueBI m)

getMarkValueB :: Mark -> BufferM MarkValue
getMarkValueB = gets . getMarkValueRaw

newMarkB :: MarkValue -> BufferM Mark
newMarkB v = queryAndModify $ newMarkBI v

deleteMarkB :: Mark -> BufferM ()
deleteMarkB m = modifyBuffer $ deleteMarkValueBI m

-- | Execute a @BufferM@ value on a given buffer, using a dummy window.  The new state of
-- the buffer is discarded.
runBufferDummyWindow :: FBuffer -> BufferM a -> a
runBufferDummyWindow b = fst . runBuffer (dummyWindow $ bkey b) b


-- | Mark the current point in the undo list as a saved state.
markSavedB :: UTCTime -> BufferM ()
markSavedB t = do (%=) undosA setSavedFilePointU
                  assign lastSyncTimeA t

bkey :: FBuffer -> BufferRef
bkey = view bkey__A

isUnchangedBuffer :: FBuffer -> Bool
isUnchangedBuffer = isAtSavedFilePointU . view undosA

startUpdateTransactionB :: BufferM ()
startUpdateTransactionB = do
  transactionPresent <- use updateTransactionInFlightA
  if transactionPresent
  then error "Already started update transaction"
  else do
    (%=) undosA $ addChangeU InteractivePoint
    assign updateTransactionInFlightA True

commitUpdateTransactionB :: BufferM ()
commitUpdateTransactionB = do
  transactionPresent <- use updateTransactionInFlightA
  if not transactionPresent
  then error "Not in update transaction"
  else do
    assign updateTransactionInFlightA False
    transacAccum <- use updateTransactionAccumA
    assign updateTransactionAccumA []

    (%=) undosA $ appEndo . mconcat $ fmap (Endo . addChangeU . AtomicChange) transacAccum
    (%=) undosA $ addChangeU InteractivePoint


undoRedo :: (forall syntax. Mark -> URList -> BufferImpl syntax -> (BufferImpl syntax, (URList, [Update])) ) -> BufferM ()
undoRedo f = do
  m <- getInsMark
  ur <- use undosA
  (ur', updates) <- queryAndModify (f m ur)
  assign undosA ur'
  tell updates

undoB :: BufferM ()
undoB = do
    isTransacPresent <- use updateTransactionInFlightA
    if isTransacPresent
    then error "Can't undo while undo transaction is in progress"
    else undoRedo undoU

redoB :: BufferM ()
redoB = do
    isTransacPresent <- use updateTransactionInFlightA
    if isTransacPresent
    then error "Can't undo while undo transaction is in progress"
    else undoRedo redoU


-- | Analogous to const, but returns a function that takes two parameters,
-- rather than one.
const2 :: t -> t1 -> t2 -> t
const2 x _ _ = x

-- | Mode applies function that always returns True.
modeAlwaysApplies :: FilePath -> String -> Bool
modeAlwaysApplies = const2 True

-- | Mode applies function that always returns False.
modeNeverApplies :: FilePath -> String -> Bool
modeNeverApplies = const2 False

emptyMode :: Mode syntax
emptyMode = Mode
  {
   modeName = "empty",
   modeApplies = modeNeverApplies,
   modeHL = ExtHL noHighlighter,
   modePrettify = const $ return (),
   modeKeymap = id,
   modeIndent = \_ _ -> return (),
   modeAdjustBlock = \_ _ -> return (),
   modeFollow = const emptyAction,
   modeIndentSettings = IndentSettings
   { expandTabs = True
   , tabSize = 8
   , shiftWidth = 4
   },
   modeToggleCommentSelection = promptCommentString,
   modeGetStrokes = \_ _ _ _ -> [],
   modeGetAnnotations = \_ _ -> [],
   modeOnLoad = return (),
   modeModeLine = defaultModeLine
  }

promptCommentString :: YiM ()
promptCommentString =
  withMinibufferFree "No comment syntax is defined. Use: " $ \cString -> do
    let c = cString ++ " "
    toggleCommentSelectionB c cString
    withBuffer $ do
      modifyMode (\x -> x { modeToggleCommentSelection =
                               toggleCommentSelectionB c cString
                          })

-- | Create buffer named @nm@ with contents @s@
newB :: BufferRef -> BufferId -> Rope -> FBuffer
newB unique nm s =
 FBuffer { bmode  = emptyMode
         , rawbuf = newBI s
         , attributes =
 Attributes { ident  = nm
            , bkey__ = unique
            , undos  = emptyU
            , preferCol = Nothing
            , bufferDynamic = def
            , pendingUpdates = []
            , selectionStyle = SelectionStyle False False
            , keymapProcess = I.End
            , winMarks = M.empty
            , lastActiveWindow = dummyWindow unique
            , lastSyncTime = epoch
            , readOnly = False
            , directoryContent = False
            , inserting = True
            , pointFollowsWindow = const False
            , updateTransactionInFlight = False
            , updateTransactionAccum = []
            } }

epoch :: UTCTime
epoch = UTCTime (toEnum 0) (toEnum 0)

-- | Point of eof
sizeB :: BufferM Point
sizeB = queryBuffer sizeBI

-- | Extract the current point
pointB :: BufferM Point
pointB = use . markPointA =<< getInsMark


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsB :: Int -> Point -> BufferM String
nelemsB n i = queryBuffer $ nelemsBI n i

nelemsB' :: Int -> Point -> BufferM Rope
nelemsB' n i = fmap (R.take n) (streamB Forward i)

streamB :: Direction -> Point -> BufferM Rope
streamB dir i = queryBuffer (getStream dir i)

indexedStreamB :: Direction -> Point -> BufferM [(Point,Char)]
indexedStreamB dir i = queryBuffer (getIndexedStream dir i)

strokesRangesB :: Maybe SearchExp -> Region -> BufferM [[Stroke]]
strokesRangesB regex r = do
    p <- pointB
    getStrokes <- withSyntaxB modeGetStrokes
    queryBuffer $ strokesRangesBI getStrokes regex r p

------------------------------------------------------------------------
-- Point based operations

-- | Move point in buffer to the given index
moveTo :: Point -> BufferM ()
moveTo x = do
  forgetPreferCol
  (.= x) . markPointA =<< getInsMark

------------------------------------------------------------------------

setInserting :: Bool -> BufferM ()
setInserting = assign insertingA

checkRO :: BufferM Bool
checkRO =  do
   ro <- use readOnlyA
   when  ro (fail "Read Only Buffer")
   return ro

applyUpdate :: Update -> BufferM ()
applyUpdate update = do
   ro    <- checkRO
   valid <- queryBuffer (isValidUpdate update)
   when  ( not ro && valid)  $ do
        forgetPreferCol
        let reversed = reverseUpdateI update
        modifyBuffer (applyUpdateI update)

        isTransacPresent <- use updateTransactionInFlightA
        if isTransacPresent
        then (%=) updateTransactionAccumA (reversed:)
        else (%=) undosA $ addChangeU $ AtomicChange reversed

        tell [update]
   -- otherwise, just ignore.

-- | Revert all the pending updates; don't touch the point.
revertPendingUpdatesB :: BufferM ()
revertPendingUpdatesB = do
  updates <- use pendingUpdatesA
  modifyBuffer (flip (foldr (\u bi -> applyUpdateI (reverseUpdateI u) bi)) [u | TextUpdate u <- updates])

-- | Write an element into the buffer at the current point.
writeB :: Char -> BufferM ()
writeB c = do
  deleteN 1
  insertB c

-- | Write the list into the buffer at current point.
writeN :: String -> BufferM ()
writeN cs = do
  off <- pointB
  deleteNAt Forward (length cs) off
  insertNAt cs off

-- | Insert newline at current point.
newlineB :: BufferM ()
newlineB = insertB '\n'

------------------------------------------------------------------------
insertNAt' :: Rope -> Point -> BufferM ()
insertNAt' rope pnt = applyUpdate (Insert pnt Forward rope)

-- | Insert the list at specified point, extending size of buffer
insertNAt :: String -> Point -> BufferM ()
insertNAt cs = insertNAt' (R.fromString cs)

-- | Insert the list at current point, extending size of buffer
insertN :: String -> BufferM ()
insertN cs = insertNAt cs =<< pointB

insertN' :: Rope -> BufferM ()
insertN' rope = insertNAt' rope =<< pointB

-- | Insert the char at current point, extending size of buffer
insertB :: Char -> BufferM ()
insertB = insertN . return

------------------------------------------------------------------------

-- | @deleteNAt n p@ deletes @n@ characters forwards from position @p@
deleteNAt :: Direction -> Int -> Point -> BufferM ()
deleteNAt dir n pos = do
  els <- R.take n <$> streamB Forward pos
  applyUpdate (Delete pos dir els)


------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLn :: BufferM Int
curLn = do
    p <- pointB
    queryBuffer (lineAt p)

-- | Return line numbers of marks
markLines :: BufferM (MarkSet Int)
markLines = mapM getLn =<< askMarks
        where getLn m = use (markPointA m) >>= lineOf


-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLn :: Int -> BufferM Int
gotoLn x = do moveTo 0
              (1 +) <$> gotoLnFrom (x - 1)

---------------------------------------------------------------------

setMode0 :: forall syntax. Mode syntax -> FBuffer -> FBuffer
setMode0 m (FBuffer _ rb at) = FBuffer m (setSyntaxBI (modeHL m) rb) at

modifyMode0 :: (forall syntax. Mode syntax -> Mode syntax) -> FBuffer -> FBuffer
modifyMode0 f (FBuffer m rb f3) = FBuffer m' (setSyntaxBI (modeHL m') rb) f3
  where m' = f m

-- | Set the mode
setAnyMode :: AnyMode -> BufferM ()
setAnyMode (AnyMode m) = setMode m

setMode :: Mode syntax -> BufferM ()
setMode m = do
  modify (setMode0 m)
  -- reset the keymap process so we use the one of the new mode.
  assign keymapProcessA I.End
  modeOnLoad m

-- | Modify the mode
modifyMode :: (forall syntax. Mode syntax -> Mode syntax) -> BufferM ()
modifyMode f = do
  modify (modifyMode0 f)
  -- reset the keymap process so we use the one of the new mode.
  assign keymapProcessA I.End

onMode :: (forall syntax. Mode syntax -> Mode syntax) -> AnyMode -> AnyMode
onMode f (AnyMode m) = AnyMode (f m)

withMode0 :: (forall syntax. Mode syntax -> a) -> FBuffer -> a
withMode0 f FBuffer {bmode = m} = f m

withModeB :: (forall syntax. Mode syntax -> BufferM a) -> BufferM a
withModeB = join . gets . withMode0

withSyntax0 :: (forall syntax. Mode syntax -> syntax -> a) -> WindowRef -> FBuffer -> a
withSyntax0 f wk (FBuffer bm rb _attrs) = f bm (getAst wk rb)


withSyntaxB :: (forall syntax. Mode syntax -> syntax -> a) -> BufferM a
withSyntaxB f = withSyntax0 f <$> askWindow wkey <*> use id


focusSyntax ::  M.Map WindowRef Region -> FBuffer -> FBuffer
focusSyntax r = modifyRawbuf (focusAst r)

withSyntaxB' :: (forall syntax. Mode syntax -> syntax -> BufferM a) -> BufferM a
withSyntaxB' = join . withSyntaxB

-- | Return indices of strings in buffer matched by regex in the
-- given region.
regexRegionB :: SearchExp -> Region -> BufferM [Region]
regexRegionB regex region = queryBuffer $ regexRegionBI regex region

-- | Return indices of next string in buffer matched by regex in the
-- given direction
regexB :: Direction -> SearchExp -> BufferM [Region]
regexB dir rx = do
  p <- pointB
  s <- sizeB
  regexRegionB rx (mkRegion p (case dir of Forward -> s; Backward -> 0))

---------------------------------------------------------------------

modifyMarkRaw :: Mark -> (MarkValue -> MarkValue) -> FBuffer -> FBuffer
modifyMarkRaw m f = modifyRawbuf $ modifyMarkBI m f

modifyMarkB :: Mark -> (MarkValue -> MarkValue) -> BufferM ()
modifyMarkB = (modify .) . modifyMarkRaw

setMarkHereB :: BufferM Mark
setMarkHereB = getMarkB Nothing

setNamedMarkHereB :: String -> BufferM ()
setNamedMarkHereB name = do
    p <- pointB
    getMarkB (Just name) >>= (.= p) . markPointA

-- | Highlight the selection
setVisibleSelection :: Bool -> BufferM ()
setVisibleSelection = assign highlightSelectionA

getInsMark :: BufferM Mark
getInsMark = insMark <$> askMarks

askMarks :: BufferM WinMarks
askMarks = do
    Just ms <- getMarks =<< ask
    return ms

getMarkB :: Maybe String -> BufferM Mark
getMarkB m = do
  p <- pointB
  queryAndModify (getMarkDefaultPosBI m p)

mayGetMarkB :: String -> BufferM (Maybe Mark)
mayGetMarkB m = queryBuffer (getMarkBI m)

-- | Move point by the given number of characters.
-- A negative offset moves backwards a positive one forward.
moveN :: Int -> BufferM ()
moveN n = do
    s <- sizeB
    moveTo =<< min s . max 0 . (+~ Size n) <$> pointB

-- | Move point -1
leftB :: BufferM ()
leftB = leftN 1

-- | Move cursor -n
leftN :: Int -> BufferM ()
leftN n = moveN (-n)

-- | Move cursor +1
rightB :: BufferM ()
rightB = rightN 1

-- | Move cursor +n
rightN :: Int -> BufferM ()
rightN = moveN

-- ---------------------------------------------------------------------
-- Line based movement and friends

-- | Move point down by @n@ lines. @n@ can be negative.
-- Returns the actual difference in lines which we moved which
-- may be negative if the requested line difference is negative.
lineMoveRel :: Int -> BufferM Int
lineMoveRel = movingToPrefCol . gotoLnFrom

movingToPrefCol :: BufferM a -> BufferM a
movingToPrefCol f = do
  prefCol <- use preferColA
  targetCol <- maybe curCol return prefCol
  r <- f
  moveToColB targetCol
  preferColA .= Just targetCol
  return r

moveToColB :: Int -> BufferM ()
moveToColB targetCol = do
  solPnt <- solPointB =<< pointB
  chrs <- nelemsB maxBound solPnt -- get all chars in the buffer, lazily.
  let cols = scanl colMove 0 chrs    -- columns corresponding to the char
      toSkip = takeWhile (\(char,col) -> char /= '\n' && col < targetCol) (zip chrs cols)
  moveTo $ solPnt +~ fromIntegral (length toSkip)

moveToLineColB :: Int -> Int -> BufferM ()
moveToLineColB line col = gotoLn line >> moveToColB col

pointOfLineColB :: Int -> Int -> BufferM Point
pointOfLineColB line col = savingPointB $ moveToLineColB line col >> pointB

forgetPreferCol :: BufferM ()
forgetPreferCol = preferColA .= Nothing

savingPrefCol :: BufferM a -> BufferM a
savingPrefCol f = do
  pc <- use preferColA
  result <- f
  preferColA .= pc
  return result

-- | Move point up one line
lineUp :: BufferM ()
lineUp = void (lineMoveRel (-1))

-- | Move point down one line
lineDown :: BufferM ()
lineDown = void (lineMoveRel 1)

-- | Return the contents of the buffer as a list
elemsB :: BufferM String
elemsB = nelemsB maxBound 0

-- | Read the character at the current point
readB :: BufferM Char
readB = pointB >>= readAtB

-- | Read the character at the given index
-- This is an unsafe operation: character NUL is returned when out of bounds
readAtB :: Point -> BufferM Char
readAtB i = do
    s <- nelemsB 1 i
    return $ case s of
               [c] -> c
               _ -> '\0'

replaceCharB :: Char -> BufferM ()
replaceCharB c = do
    writeB c
    leftB

replaceCharWithBelowB :: BufferM ()
replaceCharWithBelowB = replaceCharWithVerticalOffset 1

replaceCharWithAboveB :: BufferM ()
replaceCharWithAboveB = replaceCharWithVerticalOffset (-1)

insertCharWithBelowB :: BufferM ()
insertCharWithBelowB = maybe (return ()) insertB =<< maybeCharBelowB

insertCharWithAboveB :: BufferM ()
insertCharWithAboveB = maybe (return ()) insertB =<< maybeCharAboveB

replaceCharWithVerticalOffset :: Int -> BufferM ()
replaceCharWithVerticalOffset offset =
    maybe (return ()) replaceCharB =<< maybeCharWithVerticalOffset offset

maybeCharBelowB :: BufferM (Maybe Char)
maybeCharBelowB = maybeCharWithVerticalOffset 1

maybeCharAboveB :: BufferM (Maybe Char)
maybeCharAboveB = maybeCharWithVerticalOffset (-1)

maybeCharWithVerticalOffset :: Int -> BufferM (Maybe Char)
maybeCharWithVerticalOffset offset = savingPointB $ do
    l0 <- curLn
    c0 <- curCol
    void $ lineMoveRel offset
    l1 <- curLn
    c1 <- curCol
    curChar <- readB
    return (if c0 == c1 && l0 + offset == l1 && curChar `notElem` "\n\0"
      then Just curChar
      else Nothing)

-- | Delete @n@ characters forward from the current point
deleteN :: Int -> BufferM ()
deleteN n = pointB >>= deleteNAt Forward n

------------------------------------------------------------------------


-- | Current column.
-- Note that this is different from offset or number of chars from sol.
-- (This takes into account tabs, unicode chars, etc.)
curCol :: BufferM Int
curCol = colOf =<< pointB

colOf :: Point -> BufferM Int
colOf p = foldl' colMove 0 <$> queryBuffer (charsFromSolBI p)

lineOf :: Point -> BufferM Int
lineOf p = queryBuffer $ lineAt p

lineCountB :: BufferM Int
lineCountB = lineOf =<< sizeB

colMove :: Int -> Char -> Int
colMove col '\t' = (col + 7) `mod` 8
colMove col _    = col + 1

-- | Returns start of line point for a given point @p@
solPointB :: Point -> BufferM Point
solPointB p = queryBuffer $ solPoint' p

-- | Go to line indexed from current point
-- Returns the actual moved difference which of course
-- may be negative if the requested difference was negative.
gotoLnFrom :: Int -> BufferM Int
gotoLnFrom x = do
    l <- curLn
    p' <- queryBuffer $ solPoint (l + x)
    moveTo p'
    l' <- curLn
    return (l' - l)

-- | Access to a value into the extensible state, keyed by its type.
--   This allows you to save or retrieve inside a 'BufferM' monad, ie:
--
-- > assign bufferDynamicValueA updatedvalue
-- > value <- use bufferDynamicValueA
bufferDynamicValueA :: YiVariable a => Lens' FBuffer a
bufferDynamicValueA = bufferDynamicA . dynamicValueA

-- | perform a @BufferM a@, and return to the current point. (by using a mark)
savingExcursionB :: BufferM a -> BufferM a
savingExcursionB f = do
    m <- getMarkB Nothing
    res <- f
    moveTo =<< use (markPointA m)
    return res

markPointA :: Mark -> Lens' FBuffer Point
markPointA mark = lens getter setter where
  getter b = markPoint $ getMarkValueRaw mark b
  setter b pos = modifyMarkRaw mark (\v -> v {markPoint = pos}) b

-- | perform an @BufferM a@, and return to the current point
savingPointB :: BufferM a -> BufferM a
savingPointB f = savingPrefCol $ do
  p <- pointB
  res <- f
  moveTo p
  return res

pointAt :: forall a. BufferM a -> BufferM Point
pointAt f = savingPointB (f *> pointB)

pointAfterCursorB :: Point -> BufferM Point
pointAfterCursorB p = pointAt $ do
  moveTo p
  rightB

-- | What would be the point after doing the given action?
-- The argument must not modify the buffer.
destinationOfMoveB :: BufferM a -> BufferM Point
destinationOfMoveB f = savingPointB (f >> pointB)

-------------
-- Window

askWindow :: (Window -> a) -> BufferM a
askWindow = asks

withEveryLineB :: BufferM () -> BufferM ()
withEveryLineB action = savingPointB $ do
  lineCount <- lineCountB
  forM_ [1 .. lineCount] $ \l -> do
    void $ gotoLn l
    action

makeLensesWithSuffix "A" ''IndentSettings
makeLensesWithSuffix "A" ''Mode
