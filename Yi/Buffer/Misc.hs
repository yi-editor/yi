{-# LANGUAGE TemplateHaskell, CPP, GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification, Rank2Types, TypeSynonymInstances #-}

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
  , insertN
  , insertNAt
  , insertB
  , deleteN
  , nelemsB
  , nelemsB'
  , writeB
  , writeN
  , deleteNAt
  , deleteNBytes
  , readB
  , elemsB
  , undosA
  , undoB
  , redoB
  , getMarkB
  , mayGetMarkB
  , getMarkValueB
  , setMarkPointB
  , modifyMarkB
  , newMarkB
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
  , forgetPreferCol
  , movingToPrefCol
  , setPrefCol
  , markSavedB
  , addOverlayB
  , delOverlayB
  , delOverlayLayerB
  , savingExcursionB
  , savingPointB
  , pendingUpdatesA
  , highlightSelectionA
  , rectangleSelectionA
  , revertPendingUpdatesB
  , askWindow
  , clearSyntax
  , Mode (..)
  , AnyMode (..)
  , IndentBehaviour (..)
  , IndentSettings (..)
  , modeAlwaysApplies
  , modeNeverApplies
  , emptyMode
  , withModeB
  , withMode0
  , onMode
  , withSyntax0
  , withSyntaxB
  , keymapProcessA
  , strokesRangesB
  , streamB
  , indexedStreamB
  , getMarkPointB
  , askMarks
  , pointAt
  , pointDriveA
  , SearchExp
  , charIndexB
  , byteIndexB
  , charRegionB
  , byteRegionB
  , indexStreamRegionB
  , lastActiveWindowA
  , bufferDynamicValueA
  , shortIdentString
  , identString
  , miniIdentString
  , identA
  , BufferId
  , file
  , lastSyncTimeA
  )
where

import Prelude (ceiling, span, unzip, take, drop)
import Yi.Prelude
import Yi.Region
import System.FilePath
import Yi.Buffer.Implementation
import Yi.Region
import Yi.Syntax
import Yi.Buffer.Undo
import Yi.Dynamic
import Yi.Window
import Control.Monad.RWS.Strict hiding (mapM_, mapM, get, put, forM)
import Data.Accessor.Template
import Data.Binary
import Data.DeriveTH
import Data.Derive.Binary
import Data.List (scanl, takeWhile, zip, length)
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import {-# source #-} Yi.Keymap
import Yi.Monad
import Yi.Interact as I
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Lazy as LB
import Yi.Buffer.Basic
import Data.Time

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

data MarkSet a = MarkSet { fromMark, insMark, selMark, toMark :: !a }

instance Traversable MarkSet where
    traverse f (MarkSet a b c d) = MarkSet <$> f a <*> f b <*> f c <*> f d
instance Foldable MarkSet where
    foldMap = foldMapDefault
instance Functor MarkSet where
    fmap = fmapDefault

$(derive makeBinary ''MarkSet)


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
                , bkey__   :: !BufferRef            -- ^ immutable unique key
                , undos  :: !URList               -- ^ undo/redo list
                -- , readOnly :: !Bool                -- ^ a read-only bit (TODO)
                , pointDrive :: !Bool
                , bufferDynamic :: !DynamicValues -- ^ dynamic components
                , preferCol :: !(Maybe Int)       -- ^ prefered column to arrive at when we do a lineDown / lineUp
                , pendingUpdates :: ![UIUpdate]    -- ^ updates that haven't been synched in the UI yet
                , selectionStyle :: !SelectionStyle
                , process :: !KeymapProcess
                , winMarks :: !(M.Map WindowRef WinMarks)
                , lastActiveWindow :: !Window
                , lastSyncTime :: !UTCTime -- ^ time of the last synchronization with disk
                } deriving Typeable

$(nameDeriveAccessors ''Attributes (\n -> Just (n ++ "AA")))

-- unfortunately the dynamic stuff can't be read.
instance Binary Attributes where
    put (Attributes n b u pd _bd pc pu selectionStyle_ _proc wm law lst) = do
          put n >> put b >> put u
          put pd >> put pc >> put pu >> put selectionStyle_ >> put wm
          put law >> put lst
    get = Attributes <$> get <*> get <*> get <*> 
          get <*> pure emptyDV <*> get <*> get <*> get <*> pure I.End <*> get <*> get <*> get

instance Binary UTCTime where
    put (UTCTime x y) = put (fromEnum x) >> put (fromEnum y)
    get = UTCTime <$> (toEnum <$> get) <*> (toEnum <$> get)

data FBuffer = forall syntax.
        FBuffer { bmode  :: !(Mode syntax)
                , rawbuf :: !(BufferImpl syntax)
                , attributes :: !Attributes
               } 
        deriving Typeable


shortIdentString :: [a] -> FBuffer -> [Char]
shortIdentString prefix b = case b ^. identA of
    Left bName -> "*" ++ bName ++ "*"
    Right fName -> joinPath $ drop (length prefix) $ splitPath $ fName

identString :: FBuffer -> [Char]
identString b = case b ^. identA of
    Left bName -> "*" ++ bName ++ "*"
    Right fName -> fName

miniIdentString :: FBuffer -> [Char]
miniIdentString b = case b ^. identA of
    Right _ -> "MINIFILE:"
    Left bufName -> bufName

identA :: Accessor FBuffer BufferId
identA = identAA . attrsA




-- unfortunately the dynamic stuff can't be read.
instance Binary FBuffer where
    put (FBuffer binmode r attributes_) =
      let strippedRaw :: BufferImpl ()
          strippedRaw = (setSyntaxBI (modeHL emptyMode) r) 
      in do
          put (modeName binmode)
          put strippedRaw
          put attributes_
    get = do
        mnm <- get
        FBuffer <$> pure (emptyMode {modeName = mnm}) <*> getStripped <*> get
        where getStripped :: Get (BufferImpl ())
              getStripped = get

instance Binary SelectionStyle where
  put (SelectionStyle h r) = put h >> put r
  get = SelectionStyle <$> get <*> get

-- | udpate the syntax information (clear the dirty "flag")
clearSyntax :: FBuffer -> FBuffer
clearSyntax = modifyRawbuf updateSyntax

modifyRawbuf :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> FBuffer -> FBuffer
modifyRawbuf f (FBuffer f1 f2 f3) = (FBuffer f1 (f f2) f3)

queryAndModifyRawbuf :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) ->
                     FBuffer -> (FBuffer, x)
queryAndModifyRawbuf f (FBuffer f1 f5 f3) = 
    let (f5', x) = f f5
    in (FBuffer f1 f5' f3, x)

attrsA :: Accessor FBuffer Attributes
attrsA = accessor attributes (\a e -> case e of FBuffer f1 f2 _ -> FBuffer f1 f2 a)

-- | Use in readonly!
lastActiveWindowA :: Accessor FBuffer Window
lastActiveWindowA = lastActiveWindowAA . attrsA

lastSyncTimeA :: Accessor FBuffer UTCTime
lastSyncTimeA = lastSyncTimeAA . attrsA

pointDriveA :: Accessor FBuffer Bool
pointDriveA = pointDriveAA . attrsA

undosA :: Accessor FBuffer URList
undosA = undosAA . attrsA

file :: FBuffer -> (Maybe FilePath)
file b = case b ^. identA of
    Right f -> Just f
    _ -> Nothing

preferColA :: Accessor FBuffer (Maybe Int)
preferColA = preferColAA . attrsA

bufferDynamicA :: Accessor FBuffer DynamicValues
bufferDynamicA = bufferDynamicAA . attrsA

pendingUpdatesA :: Accessor FBuffer [UIUpdate]
pendingUpdatesA = pendingUpdatesAA . attrsA

selectionStyleA :: Accessor FBuffer SelectionStyle
selectionStyleA = selectionStyleAA . attrsA

highlightSelectionA :: Accessor FBuffer Bool
highlightSelectionA = 
  accessor highlightSelection (\x e -> e { highlightSelection = x })
  . selectionStyleA

rectangleSelectionA :: Accessor FBuffer Bool
rectangleSelectionA = 
  accessor rectangleSelection (\x e -> e { rectangleSelection = x })
  . selectionStyleA

keymapProcessA :: Accessor FBuffer KeymapProcess
keymapProcessA = processAA . attrsA

winMarksA :: Accessor FBuffer (M.Map Int WinMarks)
winMarksA = winMarksAA . attrsA

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

data Mode syntax = Mode
    {
     modeName :: String,              -- ^ so this can be serialized, debugged.
     modeApplies :: FilePath -> String -> Bool, -- ^ What type of files does this mode apply to?
     modeHL :: ExtHL syntax,          -- ^ Syntax highlighter
     modePrettify :: syntax -> BufferM (), -- ^ Prettify current "paragraph"
     modeKeymap :: KeymapEndo, -- ^ Buffer-local keymap modification
     modeIndent :: syntax -> IndentBehaviour -> BufferM (), -- ^ emacs-style auto-indent line
     modeAdjustBlock :: syntax -> Int -> BufferM (), -- ^ adjust the indentation after modification
     modeFollow :: syntax -> Action, -- ^ Follow a "link" in the file. (eg. go to location of error message)
     modeIndentSettings :: IndentSettings,
     modeToggleCommentSelection :: BufferM (),
     modeGetStrokes :: syntax -> Point -> Point -> Point -> [Stroke],
     modeGetAnnotations :: syntax -> Point -> [Span String],
     -- should this be an Action instead?
     modeOnLoad :: BufferM () -- ^ An action that is to be executed when this mode is set
    }

instance Binary (Mode syntax) where
    put = put . modeName -- we just store the modename.
    get = do n <- get
             return (emptyMode {modeName = n})

{-
  Is used to specify the behaviour of the automatic indent command.
-}
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
getModeLine prefix = do
    col <- curCol
    pos <- pointB
    ln <- curLn
    p <- pointB
    s <- sizeB
    modeNm <- gets (withMode0 modeName)
    unchanged <- gets isUnchangedBuffer
    let pct = if pos == 1 then "Top" else getPercent p s
        chg = if unchanged then "-" else "*"
    nm <- gets $ shortIdentString prefix
    return $
           chg ++ " "
           ++ nm ++
           replicate 5 ' ' ++
           "L" ++ show ln ++ "  " ++ "C" ++ show col ++
           "  " ++ pct ++
           "  " ++ modeNm ++
           "  " ++ show (fromPoint p)

-- | Given a point, and the file size, gives us a percent string
getPercent :: Point -> Point -> String
getPercent a b = show p ++ "%"
    where p = ceiling (fromIntegral a / fromIntegral b * 100 :: Double) :: Int

queryBuffer :: (forall syntax. BufferImpl syntax -> x) -> BufferM x
queryBuffer f = gets (\(FBuffer _ fb _) -> f fb)

modifyBuffer :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> BufferM ()
modifyBuffer f = modify (modifyRawbuf f)

queryAndModify :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) -> BufferM x
queryAndModify f = getsAndModify (queryAndModifyRawbuf f)

-- | Adds an "overlay" to the buffer
addOverlayB :: Overlay -> BufferM ()
addOverlayB ov = do
  modA pendingUpdatesA (++ [overlayUpdate ov])
  modifyBuffer $ addOverlayBI ov

-- | Remove an existing "overlay"
delOverlayB :: Overlay -> BufferM ()
delOverlayB ov = do
  modA pendingUpdatesA (++ [overlayUpdate ov])
  modifyBuffer $ delOverlayBI ov

delOverlayLayerB :: OvlLayer -> BufferM ()
delOverlayLayerB l = do
  modifyBuffer $ delOverlayLayer l

-- | Execute a @BufferM@ value on a given buffer and window.  The new state of
-- the buffer is returned alongside the result of the computation.
runBuffer :: Window -> FBuffer -> BufferM a -> (a, FBuffer)
runBuffer w b f = 
    let (a, _, b') = runBufferFull w b f 
    in (a, b')

getMarks :: Window -> BufferM (Maybe WinMarks)
getMarks w = do
    getsA winMarksA (M.lookup $ wkey w) 
 

runBufferFull :: Window -> FBuffer -> BufferM a -> (a, [Update], FBuffer)
runBufferFull w b f = 
    let (a, b', updates) = runRWS (fromBufferM f') w b
        f' = do
            ms <- getMarks w
            when (isNothing ms) $ do
                -- this window has no marks for this buffer yet; have to create them.
                newMarkValues <- if wkey (b ^. lastActiveWindowA) == dummyWindowKey
                    then return
                        -- no previous window, create some marks from scratch.
                         MarkSet { insMark = MarkValue 0 Forward,
                                   selMark = MarkValue 0 Backward, -- sel
                                   fromMark = MarkValue 0 Backward, -- from
                                   toMark = MarkValue 0 Forward } -- to
                    else do
                        Just mrks  <- getsA winMarksA (M.lookup $ wkey (b ^. lastActiveWindowA))
                        forM mrks getMarkValueB
                newMrks <- forM newMarkValues newMarkB
                modA winMarksA (M.insert (wkey w) newMrks)
            putA lastActiveWindowA w
            f
    in (a, updates, pendingUpdatesA ^: (++ fmap TextUpdate updates) $ b')

getMarkValueB :: Mark -> BufferM MarkValue
getMarkValueB m = fromMaybe (MarkValue 0 Forward) <$> queryBuffer (getMarkValueBI m)

newMarkB :: MarkValue -> BufferM Mark
newMarkB v = queryAndModify $ newMarkBI v

-- | Execute a @BufferM@ value on a given buffer, using a dummy window.  The new state of
-- the buffer is discarded.
runBufferDummyWindow :: FBuffer -> BufferM a -> a 
runBufferDummyWindow b = fst . runBuffer (dummyWindow $ bkey b) b


-- | Mark the current point in the undo list as a saved state.
markSavedB :: UTCTime -> BufferM ()
markSavedB t = do modA undosA setSavedFilePointU
                  putA lastSyncTimeA t

bkey :: FBuffer -> BufferRef
bkey = getVal (bkey__AA . attrsA)

isUnchangedBuffer :: FBuffer -> Bool
isUnchangedBuffer = isAtSavedFilePointU . getVal undosA


undoRedo :: (forall syntax. Mark -> URList -> BufferImpl syntax -> (BufferImpl syntax, (URList, [Update])) ) -> BufferM ()
undoRedo f = do
  m <- getInsMark
  ur <- getA undosA
  (ur', updates) <- queryAndModify (f m ur)
  putA undosA ur'
  tell updates

undoB :: BufferM ()
undoB = undoRedo undoU

redoB :: BufferM ()
redoB = undoRedo redoU

-- | Analogous to const, but returns a function that takes two parameters,
-- rather than one.
const2 :: t -> t1 -> t2 -> t
const2 x = \_ _ -> x

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
   modeToggleCommentSelection = fail "'comment selection' not defined for this mode",
   modeGetStrokes = \_ _ _ _ -> [],
   modeGetAnnotations = \_ _ -> [],
   modeOnLoad = return ()
  }

-- | Create buffer named @nm@ with contents @s@
newB :: BufferRef -> BufferId -> LB.ByteString -> FBuffer
newB unique nm s = 
 FBuffer { bmode  = emptyMode
         , rawbuf = newBI s
         , attributes =
 Attributes { ident  = nm
            , bkey__ = unique
            , undos  = emptyU
            -- , readOnly = False
            , pointDrive = True
            , preferCol = Nothing
            , bufferDynamic = emptyDV 
            , pendingUpdates = []
            , selectionStyle = SelectionStyle False False
            , process = I.End
            , winMarks = M.empty
            , lastActiveWindow = dummyWindow unique
            , lastSyncTime = epoch
            } }

epoch :: UTCTime
epoch = UTCTime (toEnum 0) (toEnum 0)

-- | Point of eof
sizeB :: BufferM Point
sizeB = queryBuffer sizeBI

-- | Extract the current point
pointB :: BufferM Point
pointB = getMarkPointB =<< getInsMark


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsB :: Int -> Point -> BufferM String
nelemsB n i = queryBuffer $ nelemsBI n i

-- | Return @n@ bytes starting at @i@ of the buffer as a list, and convert it to a string.
nelemsB' :: Size -> Point -> BufferM String
nelemsB' n i = queryBuffer $ nelemsBI' n i

streamB :: Direction -> Point -> BufferM LazyUTF8.ByteString
streamB dir i = queryBuffer (getStream dir i)

indexedStreamB :: Direction -> Point -> BufferM [(Point,Char)]
indexedStreamB dir i = queryBuffer (getIndexedStream dir i)

strokesRangesB :: Maybe SearchExp -> Region -> BufferM [[Stroke]]
strokesRangesB regex r = do
    p <- pointB
    getStrokes <- gets (withSyntax0 modeGetStrokes)
    queryBuffer $ strokesRangesBI getStrokes regex r p

------------------------------------------------------------------------
-- Point based operations

-- | Move point in buffer to the given index
moveTo :: Point -> BufferM ()
moveTo x = do
  forgetPreferCol
  flip setMarkPointB x =<< getInsMark

------------------------------------------------------------------------

applyUpdate :: Update -> BufferM ()
applyUpdate update = do
  valid <- queryBuffer (isValidUpdate update)
  when valid $ do
       forgetPreferCol
       let reversed = reverseUpdateI update
       modifyBuffer (applyUpdateI update)
       modA undosA $ addChangeU $ AtomicChange $ reversed
       tell [update]
  -- otherwise, just ignore.

-- | Revert all the pending updates; don't touch the point.
revertPendingUpdatesB :: BufferM ()
revertPendingUpdatesB = do
  updates <- getA pendingUpdatesA
  modifyBuffer (flip (foldr (\u bi -> applyUpdateI (reverseUpdateI u) bi)) [u | TextUpdate u <- updates])

-- | Write an element into the buffer at the current point.
writeB :: Char -> BufferM ()
writeB c = do
  off <- pointB
  deleteNAt Forward 1 off
  insertB c

-- | Write the list into the buffer at current point.
writeN :: String -> BufferM ()
writeN cs = do
  off <- pointB
  deleteNAt Forward (length cs) off
  insertNAt cs off

------------------------------------------------------------------------

-- | Insert the list at specified point, extending size of buffer
insertNAt :: String -> Point -> BufferM ()
insertNAt cs pnt = applyUpdate (Insert pnt Forward $ LazyUTF8.fromString cs)

-- | Insert the list at current point, extending size of buffer
insertN :: String -> BufferM ()
insertN cs = insertNAt cs =<< pointB

-- | Insert the char at current point, extending size of buffer
insertB :: Char -> BufferM ()
insertB = insertN . return

------------------------------------------------------------------------

-- | @deleteNAt n p@ deletes @n@ characters forwards from position @p@
deleteNAt :: Direction -> Int -> Point -> BufferM ()
deleteNAt dir n pos = do
  els <- nelemsB n pos
  applyUpdate (Delete pos dir $ LazyUTF8.fromString els)

-- | @deleteNBytes n p@ deletes @n@ bytes forwards from position @p@
deleteNBytes :: Direction -> Size -> Point -> BufferM ()
deleteNBytes dir n pos = do
  els <- LB.take (fromIntegral n) <$> streamB Forward pos
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
        where getLn m = getMarkPointB m >>= lineOf


-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLn :: Int -> BufferM Int
gotoLn x = do moveTo 0
              (1 +) <$> gotoLnFrom (x - 1)

---------------------------------------------------------------------

setMode0 :: forall syntax. Mode syntax -> FBuffer -> FBuffer
setMode0 m (FBuffer _ rb at) = (FBuffer m (setSyntaxBI (modeHL m) rb) at)

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
  putA keymapProcessA I.End 
  modeOnLoad m

-- | Modify the mode
modifyMode :: (forall syntax. Mode syntax -> Mode syntax) -> BufferM ()
modifyMode f = do
  modify (modifyMode0 f)
  -- reset the keymap process so we use the one of the new mode.
  putA keymapProcessA I.End 

onMode :: (forall syntax. Mode syntax -> Mode syntax) -> AnyMode -> AnyMode
onMode f (AnyMode m) = AnyMode (f m)

withMode0 :: (forall syntax. Mode syntax -> a) -> FBuffer -> a
withMode0 f FBuffer {bmode = m} = f m 


withModeB :: (forall syntax. Mode syntax -> BufferM a) -> BufferM a
withModeB f = do
    act <- gets (withMode0 f)
    act
           
withSyntax0 :: (forall syntax. Mode syntax -> syntax -> a) -> FBuffer -> a
withSyntax0 f FBuffer {bmode = m, rawbuf = rb} = f m (getAst rb)

withSyntaxB :: (forall syntax. Mode syntax -> syntax -> BufferM x) -> BufferM x
withSyntaxB f = do
    act <- gets (withSyntax0 f)
    act
           
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

-- | Set the given mark point.
setMarkPointB :: Mark -> Point -> BufferM ()
setMarkPointB m pos = modifyMarkB m (\v -> v {markPoint = pos})

modifyMarkB :: Mark -> (MarkValue -> MarkValue) -> BufferM ()
modifyMarkB m f = modifyBuffer $ modifyMarkBI m f


setVisibleSelection :: Bool -> BufferM ()
setVisibleSelection = putA highlightSelectionA

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
  p <- pointB
  nextPoint <- queryBuffer (findNextChar n p)
  moveTo nextPoint

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

setPrefCol :: Maybe Int -> BufferM ()
setPrefCol = putA preferColA

-- | Move point down by @n@ lines. @n@ can be negative.
-- Returns the actual difference in lines which we moved which
-- may be negative if the requested line difference is negative.
lineMoveRel :: Int -> BufferM Int
lineMoveRel = movingToPrefCol . gotoLnFrom

movingToPrefCol :: BufferM a -> BufferM a
movingToPrefCol f = do
  prefCol <- getA preferColA
  targetCol <- maybe curCol return prefCol
  r <- f
  moveToColB targetCol
  setPrefCol $ Just targetCol
  return r

moveToColB :: Int -> BufferM ()
moveToColB targetCol = do
  solPnt <- solPointB
  chrs <- nelemsB maxBound solPnt -- get all chars in the buffer, lazily.
  let cols = scanl colMove 0 chrs    -- columns corresponding to the char
      toSkip = takeWhile (\(char,col) -> char /= '\n' && col < targetCol) (zip chrs cols)
  moveTo =<< queryBuffer (findNextChar (length toSkip) solPnt)

moveToLineColB :: Int -> Int -> BufferM ()
moveToLineColB line col = gotoLn line >> moveToColB col

pointOfLineColB :: Int -> Int -> BufferM Point
pointOfLineColB line col = savingPointB $ moveToLineColB line col >> pointB

forgetPreferCol :: BufferM ()
forgetPreferCol = setPrefCol Nothing

savingPrefCol :: BufferM a -> BufferM a
savingPrefCol f = do
  pc <- getA preferColA
  result <- f
  setPrefCol pc
  return result

-- | Move point up one line
lineUp :: BufferM ()
lineUp = lineMoveRel (-1) >> return ()

-- | Move point down one line
lineDown :: BufferM ()
lineDown = lineMoveRel 1 >> return ()

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
colOf p = foldl colMove 0 <$> queryBuffer (charsFromSolBI p)

lineOf :: Point -> BufferM Int
lineOf p = queryBuffer $ lineAt p

colMove :: Int -> Char -> Int
colMove col '\t' = (col + 7) `mod` 8
colMove col _    = col + 1

solPointB :: BufferM Point
solPointB = do
  p <- pointB
  queryBuffer $ solPointI p

-- | Go to line indexed from current point
-- Returns the actual moved difference which of course
-- may be negative if the requested difference was negative.
gotoLnFrom :: Int -> BufferM Int
gotoLnFrom x = do
    p <- pointB
    (p',lineDiff) <- queryBuffer $ gotoLnRelI x p 
    moveTo p'
    return lineDiff

-- | Access to a value into the extensible state, keyed by its type.
--   This allows you to save or retrieve inside a 'BufferM' monad, ie:
--
-- > putA bufferDynamicValueA updatedvalue
-- > value <- getA bufferDynamicValueA
bufferDynamicValueA :: Initializable a => Accessor FBuffer a
bufferDynamicValueA = dynamicValueA . bufferDynamicA

-- | perform a @BufferM a@, and return to the current point. (by using a mark)
savingExcursionB :: BufferM a -> BufferM a
savingExcursionB f = do
    m <- getMarkB Nothing
    res <- f
    moveTo =<< getMarkPointB m
    return res

getMarkPointB :: Mark -> BufferM Point
getMarkPointB m = markPoint <$> getMarkValueB m

-- | perform an @BufferM a@, and return to the current point
savingPointB :: BufferM a -> BufferM a
savingPointB f = savingPrefCol $ do
  p <- pointB
  res <- f
  moveTo p
  return res

pointAt :: forall a. BufferM a -> BufferM Point
pointAt f = savingPointB (f *> pointB)

-------------
-- Window

askWindow :: (Window -> a) -> BufferM a
askWindow = asks

-------------
-- Character-positions

{-# DEPRECATED charIndexB, byteIndexB, charRegionB, byteRegionB "The Point type was introduced to avoid confusion between byte-index and character-index, this function defeats the purpose." #-}


-- | Convert a buffer position to a character position
charIndexB :: Point -> BufferM Point
charIndexB p =
  fromIntegral <$> length <$> takeWhile (< p) <$> fst <$> unzip <$> indexedStreamB Forward 0

-- | Convert a character position to a buffer position
byteIndexB :: Point -> BufferM Point
byteIndexB p =
  fromIntegral <$> head <$> drop (fromIntegral p) <$> fst <$> unzip <$> indexedStreamB Forward 0

-- | Convert a character position region to a buffer position region
charRegionB :: Region -> BufferM Region
charRegionB r = do
  (xs,ys) <- span (< regionStart r) <$> fst <$> unzip <$> indexedStreamB Forward 0
  return (mkSizeRegion (fromIntegral (length xs)) (fromIntegral (length (takeWhile (< regionEnd r) ys))))

-- | Convert a buffer position region to  position region
byteRegionB :: Region -> BufferM Region
byteRegionB r = do
  xs <- indexStreamRegionB r
  return $ mkRegion (head xs) (1 + last xs)

-- | Obtain an indexStreamRegion for the specified buff
indexStreamRegionB :: Region -> BufferM [Point]
indexStreamRegionB r =
  take (fromIntegral (regionSize r)) <$>
  drop (fromIntegral (regionStart r)) <$>
  fst <$> unzip <$> indexedStreamB Forward 0



