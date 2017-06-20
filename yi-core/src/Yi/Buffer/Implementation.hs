{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Buffer.Implementation
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
  , Mark, MarkValue (..)
  , Size
  , Direction (..)
  , BufferImpl (mem, marks, markNames, overlays)
  , Overlay (..)
  , mkOverlay
  , overlayUpdate
  , applyUpdateI
  , isValidUpdate
  , reverseUpdateI
  , sizeBI
  , newBI
  , solPoint
  , solPoint'
  , eolPoint'
  , charsFromSolBI
  , regexRegionBI
  , getMarkDefaultPosBI
  , modifyMarkBI
  , getMarkValueBI
  , getMarkBI
  , newMarkBI
  , deleteMarkValueBI
  , addOverlayBI
  , delOverlayBI
  , delOverlaysOfOwnerBI
  , getOverlaysOfOwnerBI
  , getStream
  , getIndexedStream
  , lineAt
  , SearchExp
  , markPointAA
  , markGravityAA
  ) where

import           GHC.Generics        (Generic)

import           Data.Array          ((!))
import           Data.Binary         (Binary (..))
import           Data.Function       (on)
import           Data.List           (groupBy)
import qualified Data.Map.Strict     as M (Map, delete, empty, findMax, insert, lookup, map, maxViewWithKey)
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as Set (Set, delete, empty, filter, insert, map, toList)
import           Data.Typeable       (Typeable)
import           Yi.Buffer.Basic
import           Yi.Regex            (RegexLike (matchAll), SearchExp, searchRegex)
import           Yi.Region           (Region (..), fmapRegion, mkRegion, nearRegion, regionSize)
import           Yi.Rope             (YiString)
import qualified Yi.Rope             as R
import           Yi.Style            (StyleName, UIStyle (hintStyle, strongHintStyle))
import           Yi.Utils            (SemiNum ((+~), (~-)), makeLensesWithSuffix, mapAdjust')


data MarkValue = MarkValue { markPoint   :: !Point
                           , markGravity :: !Direction}
               deriving (Ord, Eq, Show, Typeable, Generic)

makeLensesWithSuffix "AA" ''MarkValue

instance Binary MarkValue

type Marks = M.Map Mark MarkValue

data Overlay = Overlay
    { overlayOwner      :: !R.YiString
    , overlayBegin     :: !MarkValue
    , overlayEnd       :: !MarkValue
    , overlayStyle     :: !StyleName
    , overlayAnnotation :: !R.YiString
    }

instance Eq Overlay where
    Overlay a b c _ msg == Overlay a' b' c' _ msg' =
        a == a' && b == b' && c == c' && msg == msg'

instance Ord Overlay where
    compare (Overlay a b c _ msg) (Overlay a' b' c' _ msg')
        = mconcat
            [ compare a a'
            , compare b b'
            , compare c c'
            , compare msg msg'
            ]

instance Show Overlay where
  show (Overlay a b c _ msg) = concat
    [ "Overlay { "
    , "overlayOwner = ", show a, ", "
    , "overlayBegin = ", show b, ", "
    , "overlayEnd = ", show c, ", "
    , "overlayAnnotation = ", show msg, "}"]

data BufferImpl = FBufferData
    { mem         :: !YiString -- ^ buffer text
    , marks       :: !Marks -- ^ Marks for this buffer
    , markNames   :: !(M.Map String Mark)
    , overlays    :: !(Set.Set Overlay)
    -- ^ set of (non overlapping) visual overlay regions
    } deriving Typeable

-- Atm we can't store overlays because stylenames are functions (can't be serialized)
instance Binary BufferImpl where
    put b = put (mem b) >> put (marks b) >> put (markNames b)
    get = FBufferData <$> get <*> get <*> get <*> pure Set.empty

-- | Mutation actions (also used the undo or redo list)
--
-- For the undo/redo, we use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--
-- Note that the update direction is only a hint for moving the cursor
-- (mainly for undo purposes); the insertions and deletions are always
-- applied Forward.
--
-- Note that keeping the text does not cost much: we keep the updates in the undo list;
-- if it's a "Delete" it means we have just inserted the text in the buffer, so the update shares
-- the data with the buffer. If it's an "Insert" we have to keep the data any way.
data Update
    = Insert
    { updatePoint :: !Point
    , updateDirection :: !Direction
    , _insertUpdateString :: !YiString
    }
    | Delete
    { updatePoint :: !Point
    , updateDirection :: !Direction
    , _deleteUpdateString :: !YiString
    } deriving (Show, Typeable, Generic)

instance Binary Update

updateIsDelete :: Update -> Bool
updateIsDelete Delete {} = True
updateIsDelete Insert {} = False

updateString :: Update -> YiString
updateString (Insert _ _ s) = s
updateString (Delete _ _ s) = s

updateSize :: Update -> Size
updateSize = Size . fromIntegral . R.length . updateString

data UIUpdate = TextUpdate !Update
              | StyleUpdate !Point !Size
    deriving (Generic)
instance Binary UIUpdate

--------------------------------------------------
-- Low-level primitives.

-- | New FBuffer filled from string.
newBI :: YiString -> BufferImpl
newBI s = FBufferData s M.empty M.empty Set.empty

-- | Write string into buffer.
insertChars :: YiString -> YiString -> Point -> YiString
insertChars p cs (Point i) = left `R.append` cs `R.append` right
    where (left, right) = R.splitAt i p
{-# INLINE insertChars #-}

-- | Write string into buffer.
deleteChars :: YiString -> Point -> Size -> YiString
deleteChars p (Point i) (Size n) = left `R.append` right
    where (left, rest) = R.splitAt i p
          right = R.drop n rest
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
mapOvlMarks f (Overlay _owner s e v msg) = Overlay _owner (f s) (f e) v msg

-------------------------------------
-- * "high-level" (exported) operations

-- | Point of EOF
sizeBI :: BufferImpl -> Point
sizeBI = Point . R.length . mem

-- | Return @n@ Chars starting at @i@ of the buffer.
nelemsBI :: Int -> Point -> BufferImpl -> YiString
nelemsBI n (Point i) = R.take n . R.drop i . mem

getStream :: Direction -> Point -> BufferImpl -> YiString
getStream Forward  (Point i) = R.drop i . mem
getStream Backward (Point i) = R.reverse . R.take i . mem

-- | TODO: This guy is a pretty big bottleneck and only one function
-- uses it which in turn is only seldom used and most of the output is
-- thrown away anyway. We could probably get away with never
-- converting this to String here. The old implementation did so
-- because it worked over ByteString but we don't have to.
getIndexedStream :: Direction -> Point -> BufferImpl -> [(Point,Char)]
getIndexedStream Forward  (Point p) = zip [Point p..] . R.toString . R.drop p . mem
getIndexedStream Backward (Point p) = zip (dF (pred (Point p))) . R.toReverseString . R.take p . mem
    where
      dF n = n : dF (pred n)

-- | Create an "overlay" for the style @sty@ between points @s@ and @e@
mkOverlay :: R.YiString -> Region -> StyleName -> R.YiString -> Overlay
mkOverlay owner r =
    Overlay owner
        (MarkValue (regionStart r) Backward)
        (MarkValue (regionEnd r) Forward)

-- | Obtain a style-update for a specific overlay
overlayUpdate :: Overlay -> UIUpdate
overlayUpdate (Overlay _owner (MarkValue s _) (MarkValue e _) _ _ann) =
    StyleUpdate s (e ~- s)

-- | Add a style "overlay" between the given points.
addOverlayBI :: Overlay -> BufferImpl -> BufferImpl
addOverlayBI ov fb = fb{overlays = Set.insert ov (overlays fb)}

-- | Remove a previously added "overlay"
delOverlayBI :: Overlay -> BufferImpl -> BufferImpl
delOverlayBI ov fb = fb{overlays = Set.delete ov (overlays fb)}

delOverlaysOfOwnerBI :: R.YiString -> BufferImpl -> BufferImpl
delOverlaysOfOwnerBI owner fb =
    fb{overlays = Set.filter ((/= owner) . overlayOwner) (overlays fb)}

getOverlaysOfOwnerBI :: R.YiString -> BufferImpl -> Set.Set Overlay
getOverlaysOfOwnerBI owner fb =
    Set.filter ((== owner) . overlayOwner) (overlays fb)

------------------------------------------------------------------------
-- Point based editing

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl -> Bool
isValidUpdate u b = case u of
                    (Delete p _ _)   -> check p && check (p +~ updateSize u)
                    (Insert p _ _)   -> check p
    where check (Point x) = x >= 0 && x <= R.length (mem b)

-- | Apply a /valid/ update
applyUpdateI :: Update -> BufferImpl -> BufferImpl
applyUpdateI u fb = fb
    { mem = p'
    , marks = M.map shift (marks fb)

    -- FIXME: this is inefficient; find a way to use mapMonotonic
    -- (problem is that marks can have different gravities)
    , overlays = Set.map (mapOvlMarks shift) (overlays fb)
    }
    where (!p', !amount) = case u of
            Insert pnt _ cs -> (insertChars p cs pnt, sz)
            Delete pnt _ _  -> (deleteChars p pnt sz, negate sz)
          !sz = updateSize u
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
lineAt :: Point -- ^ Line for which to grab EOL for
       -> BufferImpl -> Int
lineAt (Point p) fb = 1 + R.countNewLines (R.take p $ mem fb)

-- | Point that starts the given line (Lines are indexed from 1)
solPoint :: Int -> BufferImpl -> Point
solPoint line fb = Point $ R.length $ fst $ R.splitAtLine (line - 1) (mem fb)

-- | Point that's at EOL. Notably, this puts you right before the
-- newline character if one exists, and right at the end of the text
-- if one does not.
eolPoint' :: Point
             -- ^ Point from which we take the line to find the EOL of
          -> BufferImpl
          -> Point
eolPoint' p@(Point ofs) fb = Point . checkEol . fst . R.splitAtLine ln $ mem fb
  where
    ln = lineAt p fb
    -- In case we're somewhere without trailing newline, we need to
    -- stay where we are
    checkEol t =
      let l' = R.length t
      in case R.last t of
          -- We're looking at EOL and we weren't asking for EOL past
          -- this point, so back up one for good visual effect
          Just '\n' | l' > ofs -> l' - 1
          -- We asked for EOL past the last newline so just go to the
          -- very end of content
          _ -> l'

-- | Get begining of the line relatively to @point@.
solPoint' :: Point -> BufferImpl -> Point
solPoint' point fb = solPoint (lineAt point fb) fb

charsFromSolBI :: Point -> BufferImpl -> YiString
charsFromSolBI pnt fb = nelemsBI (fromIntegral $ pnt - sol) sol fb
    where sol = solPoint' pnt fb

-- | Return indices of all strings in buffer matching regex, inside the given region.
regexRegionBI :: SearchExp -> Region -> BufferImpl -> [Region]
regexRegionBI se r fb = case dir of
     Forward  -> fmap (fmapRegion addPoint . matchedRegion) $ matchAll' $ R.toString        bufReg
     Backward -> fmap (fmapRegion subPoint . matchedRegion) $ matchAll' $ R.toReverseString bufReg
    where matchedRegion arr = let (off,len) = arr!0 in mkRegion (Point off) (Point (off+len))
          addPoint (Point x) = Point (p + x)
          subPoint (Point x) = Point (q - x)
          matchAll' = matchAll (searchRegex dir se)
          dir = regionDirection r
          Point p = regionStart r
          Point q = regionEnd r
          Size s = regionSize r
          bufReg = R.take s . R.drop p $ mem fb

newMarkBI :: MarkValue -> BufferImpl -> (BufferImpl, Mark)
newMarkBI initialValue fb =
    let maxId = fromMaybe 0 $ markId . fst . fst <$> M.maxViewWithKey (marks fb)
        newMark = Mark $ maxId + 1
        fb' = fb { marks = M.insert newMark initialValue (marks fb)}
    in (fb', newMark)

getMarkValueBI :: Mark -> BufferImpl -> Maybe MarkValue
getMarkValueBI m (FBufferData { marks = marksMap } ) = M.lookup m marksMap

deleteMarkValueBI :: Mark -> BufferImpl -> BufferImpl
deleteMarkValueBI m fb = fb { marks = M.delete m (marks fb) }

getMarkBI :: String -> BufferImpl -> Maybe Mark
getMarkBI name FBufferData {markNames = nms} = M.lookup name nms

-- | Modify a mark value.
modifyMarkBI :: Mark -> (MarkValue -> MarkValue) -> (BufferImpl -> BufferImpl)
modifyMarkBI m f fb = fb {marks = mapAdjust' f m (marks fb)}
-- NOTE: we must insert the value strictly otherwise we can hold to whatever structure the value of the mark depends on.
-- (often a whole buffer)

------------------------------------------------------------------------

-- | Returns the requested mark, creating a new mark with that name (at the supplied position) if needed
getMarkDefaultPosBI :: Maybe String -> Point -> BufferImpl -> (BufferImpl, Mark)
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