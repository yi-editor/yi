{-# OPTIONS -cpp #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving, ExistentialQuantification, Rank2Types #-}

-- Copyright (C) 2004, 2007 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (C) 2007, 2008 JP Bernardy

-- | The 'Buffer' module defines monadic editing operations over one-dimensional
-- buffers, maintaining a current /point/.

module Yi.Buffer
  ( FBuffer (..)
  , BufferM (..)
  , WinMarks (..)
  , getMarks
  , runBuffer
  , runBufferFull
  , runBufferDummyWindow
  , keyB
  , curLn
  , curCol
  , sizeB
  , pointB
  , moveTo
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
  , getMarkValueB
  , setMarkPointB
  , modifyMarkB
  , newMarkB
  , setVisibleSelection
  , isUnchangedB
  , isUnchangedBuffer
  , setAnyMode
  , setMode
  , modifyMode
  , regexB
  , searchB
  , readAtB
  , getModeLine
  , getPercent
  , forgetPreferCol
  , markSavedB
  , addOverlayB
  , delOverlayB
  , delOverlayLayerB
  , getDynamicB
  , setDynamicB
  , savingExcursionB
  , savingPointB
  , pendingUpdatesA
  , highlightSelectionA
  , revertPendingUpdatesB
  , askWindow
  , clearSyntax
  , Mode (..)
  , AnyMode (..)
  , IndentBehaviour (..)
  , IndentSettings (..)
  , emptyMode
  , withModeB
  , onMode
  , withSyntax0
  , keymapProcessA
  , strokesRangesB
  , streamB
  , getMarkPointB
  , module Yi.Buffer.Basic
  , pointAt
  , fileA
  , nameA
  , pointDriveA
  )
where

import Prelude (ceiling)
import Yi.Prelude
import Yi.Region
import System.FilePath
import Yi.Regex (SearchExp)
import Yi.Accessor
import Yi.Buffer.Implementation
import Yi.Config
import Yi.Region
import Yi.Syntax
import Yi.Undo
import Yi.Dynamic
import Yi.Window
import Control.Monad.RWS.Strict hiding (mapM_, mapM, get, put)
import Data.Binary
import Data.List (scanl, takeWhile, zip, length)
import qualified Data.Map as M
import Data.Typeable
import {-# source #-} Yi.Keymap
import Yi.Monad
import Yi.Interact as I
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Lazy as LB
import Yi.Buffer.Basic

#ifdef TESTING
import Test.QuickCheck hiding (Config)
import Driver ()

-- TODO: make this compile.

-- instance Arbitrary FBuffer where
--     arbitrary = do b0 <- return (newB 0 "*buffername*") `ap` (LazyUTF8.fromString `liftM` arbitrary)
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

data WinMarks = WinMarks { fromMark, insMark, selMark, toMark :: !Mark }

instance Binary WinMarks where
    put (WinMarks f i s t) = put f >> put i >> put s >> put t
    get = WinMarks <$> get <*> get <*> get <*> get

data FBuffer = forall syntax.
        FBuffer { name   :: !String               -- ^ immutable buffer name
                , bkey   :: !BufferRef            -- ^ immutable unique key
                , file   :: !(Maybe FilePath)     -- ^ maybe a filename associated with this buffer. Filename is canonicalized.
                , undos  :: !URList               -- ^ undo/redo list
                , rawbuf :: !(BufferImpl syntax)
                , bmode  :: !(Mode syntax)
                -- , readOnly :: !Bool                -- ^ a read-only bit (TODO)
                , pointDrive :: !Bool
                , bufferDynamic :: !DynamicValues -- ^ dynamic components
                , preferCol :: !(Maybe Int)       -- ^ prefered column to arrive at when we do a lineDown / lineUp
                , pendingUpdates :: ![UIUpdate]    -- ^ updates that haven't been synched in the UI yet
                , highlightSelection :: !Bool
                , process :: !KeymapProcess
                , winMarks :: !(M.Map WindowRef WinMarks)
                }
        deriving Typeable

-- unfortunately the dynamic stuff can't be read.
instance Binary FBuffer where
    put (FBuffer n b f u r bmode pd _bd pc pu hs _proc wm)  =
      let strippedRaw :: BufferImpl ()
          strippedRaw = (setSyntaxBI (modeHL emptyMode) r) 
      in put n >> put b >> put f >> put u >> put strippedRaw >> put pd >> put pc >> put pu >> put hs >> put wm
    
    get = FBuffer <$> get <*> get <*> get <*> get <*> getStripped <*> get <*> get <*> pure emptyDV <*> get <*> get <*> get <*> pure I.End <*> get
        where getStripped :: Get (BufferImpl ())
              getStripped = get

-- | udpate the syntax information (clear the dirty "flag")
clearSyntax :: FBuffer -> FBuffer
clearSyntax = modifyRawbuf updateSyntax

modifyRawbuf :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> FBuffer -> FBuffer
modifyRawbuf f (FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13) = 
    (FBuffer f1 f2 f3 f4 (f f5) f6 f7 f8 f9 f10 f11 f12 f13)

queryAndModifyRawbuf :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) ->
                     FBuffer -> (FBuffer, x)
queryAndModifyRawbuf f (FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13) = 
    let (f5', x) = f f5
    in (FBuffer f1 f2 f3 f4 f5' f6 f7 f8 f9 f10 f11 f12 f13, x)

pointDriveA :: Accessor FBuffer Bool
pointDriveA = Accessor pointDrive (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 (f f7) f8 f9 f10 f11 f12 f13)


undosA :: Accessor (FBuffer) (URList)
undosA = Accessor undos (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 (f f4) f5 f6 f7 f8 f9 f10 f11 f12 f13)

fileA :: Accessor (FBuffer) (Maybe FilePath)
fileA = Accessor file (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 (f f3) f4 f5 f6 f7 f8 f9 f10 f11 f12 f13)

preferColA :: Accessor (FBuffer) (Maybe Int)
preferColA = Accessor preferCol (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 f8 (f f9) f10 f11 f12 f13)

bufferDynamicA :: Accessor (FBuffer) (DynamicValues)
bufferDynamicA = Accessor bufferDynamic (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 (f f8) f9 f10 f11 f12 f13)

pendingUpdatesA :: Accessor (FBuffer) ([UIUpdate])
pendingUpdatesA = Accessor pendingUpdates (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 (f f10) f11 f12 f13)

highlightSelectionA :: Accessor FBuffer Bool
highlightSelectionA = Accessor highlightSelection (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 (f f11) f12 f13)

nameA :: Accessor FBuffer String
nameA = Accessor name (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer (f f1) f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13)

keymapProcessA :: Accessor FBuffer KeymapProcess
keymapProcessA = Accessor process (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 (f f12) f13)

winMarksA :: Accessor FBuffer (M.Map Int WinMarks)
winMarksA = Accessor winMarks (\f e -> case e of 
                                   FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 -> 
                                    FBuffer f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 (f f13))



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
     modeName :: String, -- ^ so this could be serialized, debugged.
     modeApplies :: FilePath -> Bool, -- ^ What type of files does this mode apply to?
     modeHL :: ExtHL syntax,
     modePrettify :: syntax -> BufferM (),
     modeKeymap :: KeymapEndo, -- ^ Buffer-local keymap modification
     modeIndent :: syntax -> IndentBehaviour -> BufferM (),
     modeAdjustBlock :: syntax -> Int -> BufferM (),
     modeFollow :: syntax -> Action,
     modeIndentSettings :: IndentSettings
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
    deriving (Monad, Functor, MonadWriter [Update], MonadState FBuffer, MonadReader Window, Typeable1)

deriving instance Typeable4 RWS

instance Applicative BufferM where
    pure = return
    (<*>) = ap

instance Eq FBuffer where
   FBuffer { bkey = u } == FBuffer { bkey = v } = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer { bkey = u, name = f, undos = us }) = showString $ "Buffer #" ++ show u ++ " (" ++ show f ++ "..." ++ show us ++ ")"

-- | Given a buffer, and some information update the modeline
--
-- N.B. the contents of modelines should be specified by user, and
-- not hardcoded.
--
getModeLine :: BufferM String
getModeLine = do
    col <- curCol
    pos <- pointB
    ln <- curLn
    p <- pointB
    s <- sizeB
    modeNm <- withModeB modeName
    unchanged <- isUnchangedB
    let pct = if pos == 1 then "Top" else getPercent p s
        chg = if unchanged then "-" else "*"
    nm <- gets name
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
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int

queryBuffer :: (forall syntax. BufferImpl syntax -> x) -> (BufferM x)
queryBuffer f = gets (\(FBuffer _ _ _ _ fb _ _ _ _ _ _ _ _) -> f fb)

modifyBuffer :: (forall syntax. BufferImpl syntax -> BufferImpl syntax) -> BufferM ()
modifyBuffer f = modify (modifyRawbuf f)

queryAndModify :: (forall syntax. BufferImpl syntax -> (BufferImpl syntax,x)) -> BufferM x
queryAndModify f = getsAndModify (queryAndModifyRawbuf f)

-- | Adds an "overlay" to the buffer
addOverlayB :: Overlay -> BufferM ()
addOverlayB ov = do
  modifyA pendingUpdatesA (++ [overlayUpdate ov])
  modifyBuffer $ addOverlayBI ov

-- | Remove an existing "overlay"
delOverlayB :: Overlay -> BufferM ()
delOverlayB ov = do
  modifyA pendingUpdatesA (++ [overlayUpdate ov])
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
            (i,s) <- case ms of
               Just (WinMarks _ i s _) -> do
                   copyMark i staticInsMark
                   copyMark s staticSelMark
                   return (i,s)
               Nothing -> do
                   -- copy marks from random window.
                   (_, WinMarks fr i s t) <- M.findMax <$> getA winMarksA
                   markValues <- mapM getMarkValueB [fr, i, s, t]
                   [newFrom, newIns, newSel, newTo] <- mapM newMarkB markValues
                   modifyA winMarksA (M.insert (wkey w) (WinMarks newFrom newIns newSel newTo))
                   return (newIns,newSel)
            f <* copyMark staticInsMark i <* copyMark staticSelMark s
    in (a, updates, modifier pendingUpdatesA (++ fmap TextUpdate updates) b')

copyMark :: Mark -> Mark -> BufferM ()
copyMark src dst = do
  p <- getMarkValueB src
  setMarkPointB dst (markPoint p)

getMarkValueB :: Mark -> BufferM MarkValue
getMarkValueB m = queryBuffer (getMarkValueBI m)

newMarkB :: MarkValue -> BufferM Mark
newMarkB v = queryAndModify $ newMarkBI v

-- | Execute a @BufferM@ value on a given buffer, using a dummy window.  The new state of
-- the buffer is discarded.
runBufferDummyWindow :: FBuffer -> BufferM a -> a 
runBufferDummyWindow b = fst . runBuffer (dummyWindow $ bkey b) b


-- | Mark the current point in the undo list as a saved state.
markSavedB :: BufferM ()
markSavedB = modifyA undosA setSavedFilePointU

keyB :: FBuffer -> BufferRef
keyB (FBuffer { bkey = u }) = u

isUnchangedB :: BufferM Bool
isUnchangedB = gets isUnchangedBuffer

isUnchangedBuffer :: FBuffer -> Bool
isUnchangedBuffer = isAtSavedFilePointU . undos


undoRedo :: (forall syntax. URList -> BufferImpl syntax -> (BufferImpl syntax, (URList, [Update])) ) -> BufferM ()
undoRedo f = do
  ur <- gets undos
  (ur', updates) <- queryAndModify (f ur)
  setA undosA ur'
  tell updates

undoB :: BufferM ()
undoB = undoRedo undoU

redoB :: BufferM ()
redoB = undoRedo redoU

emptyMode :: Mode syntax
emptyMode = Mode
  { 
   modeName = "empty",
   modeApplies = const False,
   modeHL = ExtHL noHighlighter,
   modePrettify = \_ -> return (),
   modeKeymap = id,
   modeIndent = \_ _ -> return (),
   modeAdjustBlock = \_ _ -> return (),
   modeFollow = \_ -> emptyAction,
   modeIndentSettings = IndentSettings 
   { expandTabs = True
   , tabSize = 8
   , shiftWidth = 4
   }
  }

-- | Create buffer named @nm@ with contents @s@
newB :: Config -> BufferRef -> String -> LB.ByteString -> FBuffer
newB cfg unique nm s =
    FBuffer { name   = nm
            , bkey   = unique
            , file   = Nothing          -- has name, not connected to a file
            , undos  = emptyU
            , rawbuf = newBI s
            -- , readOnly = False
            , pointDrive = True
            , bmode  = emptyMode
            , preferCol = Nothing
            , bufferDynamic = emptyDV 
            , pendingUpdates = []
            , highlightSelection = False
            , process = I.End
            , winMarks = M.singleton dummyWindowKey (WinMarks dummyFromMark staticInsMark staticSelMark dummyToMark)
            }

-- | Point of eof
sizeB :: BufferM Point
sizeB = queryBuffer sizeBI

-- | Extract the current point
pointB :: BufferM Point
pointB = queryBuffer pointBI

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsB :: Int -> Point -> BufferM [Char]
nelemsB n i = queryBuffer $ nelemsBI n i

-- | Return @n@ bytes starting at @i@ of the buffer as a list, and convert it to a string.
nelemsB' :: Size -> Point -> BufferM [Char]
nelemsB' n i = queryBuffer $ nelemsBI' n i

streamB :: Direction -> Point -> BufferM LazyUTF8.ByteString
streamB dir i = queryBuffer (getStream dir i)

strokesRangesB :: Maybe SearchExp -> Point -> Point -> BufferM [[Stroke]]
strokesRangesB regex i j = queryBuffer $ strokesRangesBI regex i j

------------------------------------------------------------------------
-- Point based operations

-- | Move point in buffer to the given index
moveTo :: Point -> BufferM ()
moveTo x = do
  forgetPreferCol
  modifyBuffer $ moveToI x

------------------------------------------------------------------------

applyUpdate :: Update -> BufferM ()
applyUpdate update = do
  valid <- queryBuffer (isValidUpdate update)
  when valid $ do
       forgetPreferCol
       let reversed = reverseUpdateI update
       modifyBuffer (applyUpdateI update)
       modifyA undosA $ addChangeU $ AtomicChange $ reversed
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
insertNAt :: [Char] -> Point -> BufferM ()
insertNAt cs pnt = applyUpdate (Insert pnt Forward $ LazyUTF8.fromString cs)

-- | Insert the list at current point, extending size of buffer
insertN :: [Char] -> BufferM ()
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
curLn = queryBuffer curLnI

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLn :: Int -> BufferM Int
gotoLn x = do moveTo 0
              (1 +) <$> gotoLnFrom (x - 1)

---------------------------------------------------------------------

-- | Return index of next (or previous) string in buffer that matches argument
searchB :: Direction -> [Char] -> BufferM (Maybe Point)
searchB dir s = queryBuffer (searchBI dir s)

setMode0 :: forall syntax. Mode syntax -> FBuffer -> FBuffer
setMode0 m (FBuffer f1 f2 f3 f4 rb _ f7 f8 f9 f10 f11 f12 f13) =
    (FBuffer f1 f2 f3 f4 (setSyntaxBI (modeHL m) rb) m f7 f8 f9 f10 f11 f12 f13)

modifyMode0 :: (forall syntax. Mode syntax -> Mode syntax) -> FBuffer -> FBuffer
modifyMode0 f (FBuffer f1 f2 f3 f4 rb m f7 f8 f9 f10 f11 f12 f13) =
    let m' = f m
    in (FBuffer f1 f2 f3 f4 (setSyntaxBI (modeHL m') rb) m' f7 f8 f9 f10 f11 f12 f13)


-- | Set the mode
setAnyMode :: AnyMode -> BufferM ()
setAnyMode (AnyMode m) = setMode m

setMode :: Mode syntax -> BufferM ()
setMode m = do
  modify (setMode0 m)
  -- reset the keymap process so we use the one of the new mode.
  setA keymapProcessA I.End 


-- | Modify the mode
modifyMode :: (forall syntax. Mode syntax -> Mode syntax) -> BufferM ()
modifyMode f = do
  modify (modifyMode0 f)
  -- reset the keymap process so we use the one of the new mode.
  setA keymapProcessA I.End 

onMode :: (forall syntax. Mode syntax -> Mode syntax) -> AnyMode -> AnyMode
onMode f (AnyMode m) = AnyMode (f m)

withMode0 :: (forall syntax. Mode syntax -> a) -> FBuffer -> a
withMode0 f FBuffer {bmode = m} = f m 


withModeB :: (forall syntax. Mode syntax -> a) -> BufferM a
withModeB f = gets (withMode0 f)
           
withSyntax0 :: (forall syntax. Mode syntax -> syntax -> a) -> FBuffer -> a
withSyntax0 f FBuffer {bmode = m, rawbuf = rb} = f m (getAst rb)
           

-- | Return indices of next string in buffer matched by regex in the
-- given direction
regexB :: Direction -> SearchExp -> BufferM [Region]
regexB dir rx = do
  p <- pointB
  s <- sizeB
  queryBuffer (regexBI rx (mkRegion p (case dir of Forward -> s; Backward -> 0)))

---------------------------------------------------------------------

-- | Set the given mark point.
setMarkPointB :: Mark -> Point -> BufferM ()
setMarkPointB m pos = modifyMarkB m (\v -> v {markPoint = pos})

modifyMarkB :: Mark -> (MarkValue -> MarkValue) -> BufferM ()
modifyMarkB m f = modifyBuffer $ modifyMarkBI m f


setVisibleSelection :: Bool -> BufferM ()
setVisibleSelection = setA highlightSelectionA

getMarkB :: Maybe String -> BufferM Mark
getMarkB m = queryAndModify (getMarkBI m)

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
setPrefCol = setA preferColA

-- | Move point down by @n@ lines. @n@ can be negative.
-- Returns the actual difference in lines which we moved which
-- may be negative if the requested line difference is negative.
lineMoveRel :: Int -> BufferM Int
lineMoveRel n = do
  prefCol <- getA preferColA
  targetCol <- case prefCol of
    Nothing -> curCol
    Just x -> return x
  ofs <- gotoLnFrom n
  gotoLnFrom 0 -- make sure we are at the start of line.
  solPnt <- pointB
  chrs <- nelemsB maxBound solPnt -- get all chars in the buffer, lazily.
  let cols = scanl colMove 0 chrs    -- columns corresponding to the char
  let toSkip = takeWhile (\(char,col) -> char /= '\n' && col < targetCol) (zip chrs cols)
  moveN (length toSkip)
  setPrefCol (Just targetCol)
  return ofs

forgetPreferCol :: BufferM ()
forgetPreferCol = setPrefCol Nothing

savingPrefCol :: BufferM a -> BufferM a
savingPrefCol f = do
  pc <- gets preferCol
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
elemsB :: BufferM [Char]
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
curCol = do 
  chars <- queryBuffer charsFromSolBI
  return (foldl colMove 0 chars)

colMove :: Int -> Char -> Int
colMove col '\t' = (col + 7) `mod` 8
colMove col _    = col + 1
        

-- | Go to line indexed from current point
-- Returns the actual moved difference which of course
-- may be negative if the requested difference was negative.
gotoLnFrom :: Int -> BufferM Int
gotoLnFrom x = queryAndModify $ gotoLnRelI x

bufferDynamicValueA :: Initializable a => Accessor FBuffer a
bufferDynamicValueA = dynamicValueA .> bufferDynamicA

getDynamicB :: Initializable a => BufferM a
getDynamicB = getA bufferDynamicValueA

-- | Insert a value into the extensible state, keyed by its type
setDynamicB :: Initializable a => a -> BufferM ()
setDynamicB = setA bufferDynamicValueA


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

