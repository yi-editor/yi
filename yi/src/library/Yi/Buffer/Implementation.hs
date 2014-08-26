{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Buffer.Implementation
-- Copyright   :  (c) Don Stewart            2004, 2005
--                    Jean-Philippe Bernardy 2007, 2008
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- 'Buffer' implementation, wrapping Rope

module Yi.Buffer.Implementation
  ( UIUpdate (..)
  , Update (..)
  , updateIsDelete
  , Point
  , Mark, MarkValue(..)
  , Size
  , Direction (..)
  , BufferImpl
  , Overlay, OvlLayer (..)
  , mkOverlay
  , overlayUpdate
  , applyUpdateI
  , isValidUpdate
  , reverseUpdateI
  , nelemsBI
  , sizeBI
  , newBI
  , solPoint
  , solPoint'
  , charsFromSolBI
  , regexRegionBI
  , getMarkDefaultPosBI
  , modifyMarkBI
  , getMarkValueBI
  , getMarkBI
  , newMarkBI
  , deleteMarkValueBI
  , setSyntaxBI
  , addOverlayBI
  , delOverlayBI
  , delOverlayLayer
  , updateSyntax
  , getAst, focusAst
  , strokesRangesBI
  , getStream
  , getIndexedStream
  , lineAt
  , SearchExp
  , markPointAA
  , markGravityAA
)
where

import Control.Applicative
import Data.Array
import Data.Binary
#if __GLASGOW_HASKELL__ < 708
import Data.DeriveTH
#else
import GHC.Generics (Generic)
#endif
import Data.List (groupBy)
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Function
import Yi.Buffer.Basic
import Yi.Regex
import Yi.Region
import Yi.Style
import Yi.Syntax
import Yi.Utils
import qualified Data.Rope as F
import Data.Rope (Rope)
import qualified Data.Map as M
import qualified Data.Set as Set

data MarkValue = MarkValue { markPoint :: !Point
                           , markGravity :: !Direction}
               deriving (Ord, Eq, Show, Typeable)

makeLensesWithSuffix "AA" ''MarkValue

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''MarkValue)
#else
deriving instance Generic MarkValue
instance Binary MarkValue
#endif

type Marks = M.Map Mark MarkValue

data HLState syntax = forall cache. HLState !(Highlighter cache syntax) !cache

data OvlLayer = UserLayer | HintLayer
  deriving (Ord, Eq)
data Overlay = Overlay {
                        overlayLayer :: OvlLayer,
                        -- underscores to avoid 'defined but not used'. Remove if desired
                        _overlayBegin :: MarkValue,
                        _overlayEnd :: MarkValue,
                        _overlayStyle :: StyleName
                       }
instance Eq Overlay where
    Overlay a b c _ == Overlay a' b' c' _ = a == a' && b == b' && c == c'

instance Ord Overlay where
    compare (Overlay a b c _) (Overlay a' b' c' _)
        = compare a a' `mappend` compare b b' `mappend` compare c c'


data BufferImpl syntax =
        FBufferData { mem        :: !Rope          -- ^ buffer text
                    , marks      :: !Marks                 -- ^ Marks for this buffer
                    , markNames  :: !(M.Map String Mark)
                    , hlCache    :: !(HLState syntax)       -- ^ syntax highlighting state
                    , overlays   :: !(Set.Set Overlay) -- ^ set of (non overlapping) visual overlay regions
                    , dirtyOffset :: !Point -- ^ Lowest modified offset since last recomputation of syntax
                    }
        deriving Typeable


dummyHlState :: HLState syntax
dummyHlState = HLState noHighlighter (hlStartState noHighlighter)

-- Atm we can't store overlays because stylenames are functions (can't be serialized)
-- TODO: ideally I'd like to get rid of overlays entirely; although we could imagine them storing mere styles.
instance Binary (BufferImpl ()) where
    put b = put (mem b) >> put (marks b) >> put (markNames b)
    get = FBufferData <$> get <*> get <*> get <*> pure dummyHlState <*> pure Set.empty <*> pure 0



-- | Mutation actions (also used the undo or redo list)
--
-- For the undo/redo, we use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--
-- Note that the update direction is only a hint for moving the cursor
-- (mainly for undo purposes); the insertions and deletions are always
-- applied Forward.
data Update = Insert {updatePoint :: !Point, updateDirection :: !Direction, insertUpdateString :: !Rope}
            | Delete {updatePoint :: !Point, updateDirection :: !Direction, deleteUpdateString :: !Rope}
              -- Note that keeping the text does not cost much: we keep the updates in the undo list;
              -- if it's a "Delete" it means we have just inserted the text in the buffer, so the update shares
              -- the data with the buffer. If it's an "Insert" we have to keep the data any way.

              deriving (Show, Typeable)

#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''Update)
#else
deriving instance Generic Update
instance Binary Update
#endif

updateIsDelete :: Update -> Bool
updateIsDelete Delete {} = True
updateIsDelete Insert {} = False

updateString :: Update -> Rope
updateString (Insert _ _ s) = s
updateString (Delete _ _ s) = s

updateSize :: Update -> Size
updateSize = Size . fromIntegral . F.length . updateString

data UIUpdate = TextUpdate !Update
              | StyleUpdate !Point !Size
#if __GLASGOW_HASKELL__ < 708
$(derive makeBinary ''UIUpdate)
#else
deriving instance Generic UIUpdate
instance Binary UIUpdate
#endif

--------------------------------------------------
-- Low-level primitives.

-- | New FBuffer filled from string.
newBI :: Rope -> BufferImpl ()
newBI s = FBufferData s M.empty M.empty dummyHlState Set.empty 0

-- | read @n@ bytes from buffer @b@, starting at @i@
readChunk :: Rope -> Size -> Point -> Rope
readChunk p (Size n) (Point i) = F.take n $ F.drop i p

-- | Write string into buffer.
insertChars :: Rope -> Rope -> Point -> Rope
insertChars p cs (Point i) = left `F.append` cs `F.append` right
    where (left,right) = F.splitAt i p
{-# INLINE insertChars #-}


-- | Write string into buffer.
deleteChars :: Rope -> Point -> Size -> Rope
deleteChars p (Point i) (Size n) = left `F.append` right
    where (left,rest) = F.splitAt i p
          right = F.drop n rest
{-# INLINE deleteChars #-}

------------------------------------------------------------------------
-- Mid-level insert/delete

-- | Shift a mark position, supposing an update at a given point, by a given amount.
-- Negative amount represent deletions.
shiftMarkValue :: Point -> Size -> MarkValue -> MarkValue
shiftMarkValue from by (MarkValue p gravity) = MarkValue shifted gravity
    where shifted | p < from  = p
                  | p == from = case gravity of
                                  Backward -> p
                                  Forward -> p'
                  | otherwise {- p > from -} = p'
              where p' = max from (p +~ by)

mapOvlMarks :: (MarkValue -> MarkValue) -> Overlay -> Overlay
mapOvlMarks f (Overlay l s e v) = Overlay l (f s) (f e) v

-------------------------------------
-- * "high-level" (exported) operations

-- | Point of EOF
sizeBI :: BufferImpl syntax -> Point
sizeBI = Point . F.length . mem

-- | Return @n@ Chars starting at @i@ of the buffer as a list
nelemsBI :: Int -> Point -> BufferImpl syntax -> String
nelemsBI n i fb = F.toString $ readChunk (mem fb) (Size n) i

getStream :: Direction -> Point -> BufferImpl syntax -> Rope
getStream Forward  (Point i) fb =             F.drop i $ mem fb
getStream Backward (Point i) fb = F.reverse $ F.take i $ mem fb

getIndexedStream :: Direction -> Point -> BufferImpl syntax -> [(Point,Char)]
getIndexedStream Forward  (Point p) fb = zip [Point p..]           $ F.toString        $ F.drop p $ mem fb
getIndexedStream Backward (Point p) fb = zip (dF (pred (Point p))) $ F.toReverseString $ F.take p $ mem fb
    where dF n = n : dF (pred n)

-- | Create an "overlay" for the style @sty@ between points @s@ and @e@
mkOverlay :: OvlLayer -> Region -> StyleName -> Overlay
mkOverlay l r = Overlay l (MarkValue (regionStart r) Backward) (MarkValue (regionEnd r) Forward)

-- | Obtain a style-update for a specific overlay
overlayUpdate :: Overlay -> UIUpdate
overlayUpdate (Overlay _l (MarkValue s _) (MarkValue e _) _) = StyleUpdate s (e ~- s)

-- | Add a style "overlay" between the given points.
addOverlayBI :: Overlay -> BufferImpl syntax -> BufferImpl syntax
addOverlayBI ov fb = fb{overlays = Set.insert ov (overlays fb)}

-- | Remove a previously added "overlay"
delOverlayBI :: Overlay -> BufferImpl syntax -> BufferImpl syntax
delOverlayBI ov fb = fb{overlays = Set.delete ov (overlays fb)}

delOverlayLayer :: OvlLayer -> BufferImpl syntax -> BufferImpl syntax
delOverlayLayer layer fb = fb{overlays = Set.filter ((/= layer) . overlayLayer) (overlays fb)}
-- FIXME: this can be really inefficient.

-- | Return style information for the range @(i,j)@ Style information
--   is derived from syntax highlighting, active overlays and current regexp.  The
--   returned list contains tuples @(l,s,r)@ where every tuple is to
--   be interpreted as apply the style @s@ from position @l@ to @r@ in
--   the buffer.  In each list, the strokes are guaranteed to be
--   ordered and non-overlapping.  The lists of strokes are ordered by
--   decreasing priority: the 1st layer should be "painted" on top.
strokesRangesBI :: (Point -> Point -> Point -> [Stroke]) ->
  Maybe SearchExp -> Region -> Point -> BufferImpl syntax -> [[Stroke]]
strokesRangesBI getStrokes regex rgn  point fb = result
  where
    i = regionStart rgn
    j = regionEnd rgn
    dropBefore = dropWhile (\s ->spanEnd s <= i)
    takeIn  = takeWhile (\s -> spanBegin s <= j)

    groundLayer = [Span i mempty j]

    -- zero-length spans seem to break stroking in general, so filter them out!
    syntaxHlLayer = filter (\(Span b _m a) -> b /= a)  $ getStrokes point i j

    layers2 = map (map overlayStroke) $ groupBy ((==) `on` overlayLayer) $  Set.toList $ overlays fb
    layer3 = case regex of
               Just re -> takeIn $ map hintStroke $ regexRegionBI re (mkRegion i j) fb
               Nothing -> []
    result = map (map clampStroke . takeIn . dropBefore) (layer3 : layers2 ++ [syntaxHlLayer, groundLayer])
    overlayStroke (Overlay _ sm  em a) = Span (markPoint sm) a (markPoint em)
    clampStroke (Span l x r) = Span (max i l) x (min j r)
    hintStroke r = Span (regionStart r) (if point `nearRegion` r then strongHintStyle else hintStyle) (regionEnd r)

------------------------------------------------------------------------
-- Point based editing

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl syntax -> Bool
isValidUpdate u b = case u of
                    (Delete p _ _)   -> check p && check (p +~ updateSize u)
                    (Insert p _ _)   -> check p
    where check (Point x) = x >= 0 && x <= F.length (mem b)


-- | Apply a /valid/ update
applyUpdateI :: Update -> BufferImpl syntax -> BufferImpl syntax
applyUpdateI u fb = touchSyntax (updatePoint u) $
                    fb {mem = p', marks = M.map shift (marks fb),
                                   overlays = Set.map (mapOvlMarks shift) (overlays fb)}
                                   -- FIXME: this is inefficient; find a way to use mapMonotonic
                                   -- (problem is that marks can have different gravities)
    where (p', amount) = case u of
                           Insert pnt _ cs -> (insertChars p cs pnt, sz)
                           Delete pnt _ _  -> (deleteChars p pnt sz, negate sz)
          sz = updateSize u
          shift = shiftMarkValue (updatePoint u) amount
          p = mem fb
          -- FIXME: remove collapsed overlays

-- | Reverse the given update
reverseUpdateI :: Update -> Update
reverseUpdateI (Delete p dir cs) = Insert p (reverseDir dir) cs
reverseUpdateI (Insert p dir cs) = Delete p (reverseDir dir) cs


------------------------------------------------------------------------
-- Line based editing

-- | Line at the given point. (Lines are indexed from 1)
lineAt :: Point -> BufferImpl syntax -> Int
lineAt (Point point) fb = 1 + F.countNewLines (F.take point (mem fb))

-- | Point that starts the given line (Lines are indexed from 1)
solPoint :: Int -> BufferImpl syntax -> Point
solPoint line fb = Point $ F.length $ fst $ F.splitAtLine (line - 1) (mem fb)

-- | Get begin of the line relatively to @point@.
solPoint' :: Point -> BufferImpl syntax -> Point
solPoint' point fb = solPoint (lineAt point fb) fb


charsFromSolBI :: Point -> BufferImpl syntax -> String
charsFromSolBI pnt fb = nelemsBI (fromIntegral $ pnt - sol) sol fb
    where sol = solPoint' pnt fb

-- | Return indices of all strings in buffer matching regex, inside the given region.
regexRegionBI :: SearchExp -> Region -> forall syntax. BufferImpl syntax -> [Region]
regexRegionBI se r fb = case dir of
     Forward  -> fmap (fmapRegion addPoint . matchedRegion) $ matchAll' $ F.toString        bufReg
     Backward -> fmap (fmapRegion subPoint . matchedRegion) $ matchAll' $ F.toReverseString bufReg
    where matchedRegion arr = let (off,len) = arr!0 in mkRegion (Point off) (Point (off+len))
          addPoint (Point x) = Point (p + x)
          subPoint (Point x) = Point (q - x)
          matchAll' = matchAll (searchRegex dir se)
          dir = regionDirection r
          Point p = regionStart r
          Point q = regionEnd r
          Size s = regionSize r
          bufReg = F.take s $ F.drop p $ mem fb

newMarkBI :: MarkValue -> BufferImpl syntax -> (BufferImpl syntax, Mark)
newMarkBI initialValue fb =
    let maxId = fromMaybe 0 $ markId . fst . fst <$> M.maxViewWithKey (marks fb)
        newMark = Mark $ maxId + 1
        fb' = fb { marks = M.insert newMark initialValue (marks fb)}
    in (fb', newMark)

getMarkValueBI :: Mark -> BufferImpl syntax -> Maybe MarkValue
getMarkValueBI m (FBufferData { marks = marksMap } ) = M.lookup m marksMap

deleteMarkValueBI :: Mark -> BufferImpl syntax -> BufferImpl syntax
deleteMarkValueBI m fb = fb { marks = M.delete m (marks fb) }

getMarkBI :: String -> BufferImpl syntax -> Maybe Mark
getMarkBI name FBufferData {markNames = nms} = M.lookup name nms

-- | Modify a mark value.
modifyMarkBI :: Mark -> (MarkValue -> MarkValue) -> (forall syntax. BufferImpl syntax -> BufferImpl syntax)
modifyMarkBI m f fb = fb {marks = mapAdjust' f m (marks fb)}
-- NOTE: we must insert the value strictly otherwise we can hold to whatever structure the value of the mark depends on.
-- (often a whole buffer)

setSyntaxBI :: ExtHL syntax -> BufferImpl oldSyntax -> BufferImpl syntax
setSyntaxBI (ExtHL e) fb = touchSyntax 0 $ fb {hlCache = HLState e (hlStartState e)}

touchSyntax ::  Point -> BufferImpl syntax -> BufferImpl syntax
touchSyntax touchedIndex fb = fb { dirtyOffset = min touchedIndex (dirtyOffset fb)}

updateSyntax :: BufferImpl syntax -> BufferImpl syntax
updateSyntax fb@FBufferData {dirtyOffset = touchedIndex, hlCache = HLState hl cache}
    | touchedIndex == maxBound = fb
    | otherwise
    = fb {dirtyOffset = maxBound,
          hlCache = HLState hl (hlRun hl getText touchedIndex cache)
         }
    where getText = Scanner 0 id (error "getText: no character beyond eof")
                     (\idx -> getIndexedStream Forward idx fb)

------------------------------------------------------------------------

-- | Returns the requested mark, creating a new mark with that name (at the supplied position) if needed
getMarkDefaultPosBI :: Maybe String -> Point -> BufferImpl syntax -> (BufferImpl syntax, Mark)
getMarkDefaultPosBI name defaultPos fb@FBufferData {marks = mks, markNames = nms} =
  case flip M.lookup nms =<< name of
    Just m' -> (fb, m')
    Nothing ->
           let newMark = Mark (1 + max 1 (markId $ fst (M.findMax mks)))
               nms' = case name of
                        Nothing -> nms
                        Just nm -> M.insert nm newMark nms
               mks' = M.insert newMark (MarkValue defaultPos Forward) mks
           in (fb {marks = mks', markNames = nms'}, newMark)


getAst :: WindowRef -> BufferImpl syntax -> syntax
getAst w FBufferData {hlCache = HLState (SynHL {hlGetTree = gt}) cache} = gt cache w

focusAst ::  M.Map WindowRef Region -> BufferImpl syntax -> BufferImpl syntax
focusAst r b@FBufferData {hlCache = HLState s@(SynHL {hlFocus = foc}) cache} = b {hlCache = HLState s (foc r cache)}
