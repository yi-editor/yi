{-# LANGUAGE PatternGuards, ExistentialQuantification, DeriveDataTypeable, Rank2Types, FlexibleContexts, FlexibleInstances #-}

-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2007-8 JP Bernardy

-- | 'Buffer' implementation, wrapping ByteRope
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
  , nelemsBI'
  , sizeBI
  , newBI
  , solPointI
  , gotoLnRelI
  , charsFromSolBI
  , regexRegionBI
  , getMarkDefaultPosBI
  , modifyMarkBI
  , getMarkValueBI
  , getMarkBI
  , newMarkBI
  , setSyntaxBI
  , addOverlayBI
  , delOverlayBI
  , delOverlayLayer
  , inBounds
  , findNextChar
  , updateSyntax
  , getAst
  , strokesRangesBI
  , toIndexedString
  , getStream
  , getIndexedStream
  , newLine
  , lineAt
  , SearchExp
)
where

import Yi.Prelude
import Prelude (take, takeWhile, dropWhile, map, reverse)
import Yi.Syntax 

import qualified Data.Map as M
import Data.Binary
import Data.Maybe 
import Data.Monoid
import Yi.Style

import Control.Monad

import Yi.Regex

import qualified Data.ByteRope as F
import Data.ByteRope (ByteRope)
import qualified Data.ByteString.Lazy as LazyB
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Codec.Binary.UTF8.Generic as UF8Codec
import Yi.Buffer.Basic
import Data.Array
import Data.Char
import Data.Maybe
import Data.List (groupBy, drop)
import Data.Word
import qualified Data.Set as Set
import Data.Typeable
import Yi.Region

data MarkValue = MarkValue {markPoint :: !Point, markGravity :: !Direction}
               deriving (Ord, Eq, Show, Typeable {-! Binary !-})

type Marks = M.Map Mark MarkValue

data HLState syntax = forall cache. HLState !(Highlighter cache syntax) !cache

data OvlLayer = UserLayer | HintLayer
  deriving (Ord, Eq)
data Overlay = Overlay {
                        overlayLayer :: OvlLayer,
                        overlayBegin :: MarkValue,
                        overlayEnd :: MarkValue,
                        overlayStyle :: StyleName
                       }
instance Eq Overlay where
    Overlay a b c _ == Overlay a' b' c' _ = a == a' && b == b' && c == c'

instance Ord Overlay where
    compare (Overlay a b c _) (Overlay a' b' c' _) 
        = compare a a' `mappend` compare b b' `mappend` compare c c'


data BufferImpl syntax =
        FBufferData { mem        :: !ByteRope          -- ^ buffer text
                    , marks      :: !Marks                 -- ^ Marks for this buffer
                    , markNames  :: !(M.Map String Mark)
                    , hlCache    :: !(HLState syntax)       -- ^ syntax highlighting state
                    , overlays   :: !(Set.Set Overlay) -- ^ set of (non overlapping) visual overlay regions
                    , dirtyOffset :: !Point -- ^ Lowest modified offset since last recomputation of syntax 
                    }
        deriving Typeable

-- Atm we can't store overlays because stylenames are functions (can't be serialized)
-- TODO: ideally I'd like to get rid of overlays entirely; although we could imagine them storing mere styles.
instance Binary (BufferImpl ()) where
    put b = put (mem b) >> put (marks b) >> put (markNames b)
    get = pure FBufferData <*> get <*> get <*> get <*> pure dummyHlState <*> pure Set.empty <*> pure 0
    
    

-- | Mutation actions (also used the undo or redo list)
--
-- For the undo/redo, we use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--
-- Note that the update direction is only a hint for moving the cursor
-- (mainly for undo purposes); the insertions and deletions are always
-- applied Forward.
data Update = Insert {updatePoint :: !Point, updateDirection :: !Direction, insertUpdateString :: !LazyB.ByteString} 
            | Delete {updatePoint :: !Point, updateDirection :: !Direction, deleteUpdateString :: !LazyB.ByteString}
              -- Note that keeping the text does not cost much: we keep the updates in the undo list;
              -- if it's a "Delete" it means we have just inserted the text in the buffer, so the update shares
              -- the data with the buffer. If it's an "Insert" we have to keep the data any way.

              deriving (Show, Typeable {-! Binary !-})

updateIsDelete :: Update -> Bool
updateIsDelete Delete {} = True
updateIsDelete Insert {} = False

updateString :: Update -> LazyUTF8.ByteString
updateString (Insert _ _ s) = s
updateString (Delete _ _ s) = s

updateSize :: Update -> Size
updateSize = Size . fromIntegral . LazyB.length . updateString

data UIUpdate = TextUpdate !Update
              | StyleUpdate !Point !Size
 deriving ({-! Binary !-})

--------------------------------------------------
-- Low-level primitives.

dummyHlState :: HLState syntax
dummyHlState = (HLState noHighlighter (hlStartState noHighlighter))

-- | New FBuffer filled from string.
newBI :: LazyB.ByteString -> BufferImpl ()
newBI s = FBufferData (F.fromLazyByteString s) M.empty M.empty dummyHlState Set.empty 0

-- | read @n@ chars from buffer @b@, starting at @i@
readChars :: ByteRope -> Int -> Point -> String
readChars p n (Point i) = take n $ LazyUTF8.toString $ F.toLazyByteString $ F.drop i $ p
{-# INLINE readChars #-}

-- | read @n@ bytes from buffer @b@, starting at @i@
readChunk :: ByteRope -> Size -> Point -> ByteRope
readChunk p (Size n) (Point i) = F.take n $ F.drop i $ p

-- | Write string into buffer.
insertChars :: ByteRope -> ByteRope -> Point -> ByteRope
insertChars p cs (Point i) = left `F.append` cs `F.append` right
    where (left,right) = F.splitAt i p
{-# INLINE insertChars #-}


-- | Write string into buffer.
deleteChars :: ByteRope -> Point -> Size -> ByteRope
deleteChars p (Point i) (Size n) = left `F.append` right
    where (left,rest) = F.splitAt i p
          right = F.drop n rest
{-# INLINE deleteChars #-}

-- | calculate whether a move is in bounds.
-- Note that one can move to 1 char past the end of the buffer.
inBounds :: Point -> Point -> Point
inBounds i end | i <= 0    = 0
               | i > end   = max 0 end
               | otherwise = i
{-# INLINE inBounds #-}

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
nelemsBI n i fb = readChars (mem fb) n i

nelemsBI' :: Size -> Point -> BufferImpl syntax -> String
nelemsBI' n i fb = LazyUTF8.toString $ F.toLazyByteString $ readChunk (mem fb) n i

getStream :: Direction -> Point -> BufferImpl syntax -> LazyUTF8.ByteString
getStream Forward  (Point i) fb = F.toLazyByteString        $ F.drop i $ mem $ fb
getStream Backward (Point i) fb = F.toReverseLazyByteString $ F.take i $ mem $ fb

getIndexedStream :: Direction -> Point -> BufferImpl syntax -> [(Point,Char)]
getIndexedStream Forward  (Point i) fb = toIndexedString Forward  (Point i) $ F.toLazyByteString        $ F.drop i $ mem $ fb
getIndexedStream Backward (Point i) fb = toIndexedString Backward (Point i) $ F.toReverseLazyByteString $ F.take i $ mem $ fb


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
strokesRangesBI getStrokes regex rgn  point fb@(FBufferData {hlCache = HLState hl cache}) = result
  where
    i = regionStart rgn
    j = regionEnd rgn
    dropBefore = dropWhile (\s ->spanEnd s <= i)
    takeIn  = takeWhile (\s -> spanBegin s <= j)

    groundLayer = [(Span i mempty j)]
    syntaxHlLayer = getStrokes point i j 
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

findNextChar :: Int -> Point -> BufferImpl syntax -> Point
findNextChar m p fb 
    | m < 0 = case drop (0 - 1 - m) (getIndexedStream Backward p fb) of
        [] -> 0
        (i,_):_ -> i
    | otherwise = case drop m (getIndexedStream Forward p fb) of
        [] -> sizeBI fb
        (i,_):_ -> i

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
                           Insert pnt _ cs -> (insertChars p (F.fromLazyByteString cs) pnt, sz)
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

ord' :: Char -> Word8
ord' = fromIntegral . ord

newLine :: Word8
newLine = ord' '\n'

lineAt :: Point -> BufferImpl syntax -> Int
lineAt point fb = 1 + F.count newLine (F.take (fromPoint point) (mem fb))

-- | Get begin of the line relatively to @point@.
solPointI :: Point -> BufferImpl syntax -> Point
solPointI (Point point) = Point . maybe 0 (+1) . F.elemIndexEnd newLine . F.take point . mem

-- | Get the point at line number @n@, relatively from @point@. @0@ will go to
-- the start of this line. Returns the actual line difference we went
-- to (which may be not be the requested one, if it was out of range)
-- Note that the line-difference returned will be negative if we are
-- going backwards to previous lines (that is if @n@ was negative).
gotoLnRelI :: Int -> Point -> BufferImpl syntax -> (Point, Int)
gotoLnRelI n (Point point) fb = (Point newPoint, difference)
  where
  -- The text of the buffer
  s     = mem fb
  -- The current point that we are at in the buffer.
  (difference, newPoint)
    -- Important that we go up if it is 0 since findDownLine
    -- fails for zero.
    | n <= 0    = findUpLine 0 n upLineStarts
    | otherwise = findDownLine 1 n downLineStarts

  -- The offsets of all line beginnings above the current point.
  upLineStarts = map (+1) (F.elemIndicesEnd newLine (F.take point s)) ++ [0]

  -- Go up to find the line we wish for the returned value is a pair
  -- consisting of the point of the start of the line to which we move
  -- and the difference in lines we have moved (negative here if we move at all)
  findUpLine :: Int -> Int -> [ Int ] -> (Int, Int)
  findUpLine acc _ [x]    = (acc, x)
  findUpLine acc 0 (x:_)  = (acc, x)
  findUpLine acc l (_:xs) = findUpLine (acc - 1) (l + 1) xs
  findUpLine _ _ []       =
    error $ "we append [0] to the end of upLineStarts" ++
            " hence this cannot happen."

  -- The offsets of all the line beginnings below the current point
  -- so we drop everything before the point, find all the indices of
  -- the newlines from there and add the point (plus 1) to them.
  downLineStarts = map (+(1 + point)) (F.elemIndices newLine $ F.drop point s)

  -- Go down to find the line we wish for, the returned value is a pair
  -- consisting of the point of the start of the line to which we move
  -- and the number of lines we have actually moved.
  -- Note: that this doesn't work if the number of lines to move down
  -- is zero. 
  findDownLine :: Int -> Int -> [ Int ] -> (Int, Int)
  -- try to go forward, but there is no such line
  -- this cannot happen on a recursive call so it can only happen if
  -- we started on the last line, so we return the current point.
  findDownLine acc _ []     = (acc, point) 
  findDownLine acc _ [x]    = (acc, x)
  findDownLine acc 1 (x:_)  = (acc, x)
  findDownLine acc l (_:xs) = findDownLine (acc + 1) (l - 1) xs

charsFromSolBI :: Point -> BufferImpl syntax -> String
charsFromSolBI (Point pnt) fb = LazyUTF8.toString $ F.toLazyByteString $ readChunk ptr (Size (pnt - sol)) (Point sol)
  where sol = maybe 0 (1 +) (F.elemIndexEnd newLine (F.take pnt ptr))
        ptr = mem fb


-- | Return indices of all strings in buffer matching regex, inside the given region.
regexRegionBI :: SearchExp -> Region -> forall syntax. BufferImpl syntax -> [Region]
regexRegionBI se r fb = case dir of
     Forward  -> fmap (fmapRegion addPoint . matchedRegion) $ matchAll' $ F.toLazyByteString bufReg
     Backward -> fmap (fmapRegion subPoint . matchedRegion) $ matchAll' $ F.toReverseLazyByteString bufReg
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
                     (\idx -> toIndexedString Forward idx (F.toLazyByteString (F.drop (fromPoint idx) (mem fb))))

toIndexedString :: Direction -> Point -> LazyB.ByteString -> [(Point, Char)]
toIndexedString dir p = (case dir of
    Forward -> toIndexedStringForward
    Backward -> toIndexedStringBackward) p . LazyB.unpack

toIndexedStringForward :: Point -> [Word8] -> [(Point, Char)]
toIndexedStringForward curIdx bs = 
    case UF8Codec.decode bs of
      Nothing -> []
      Just (c,n) -> let newIndex = curIdx + fromIntegral n in
                    (curIdx,c) : (newIndex `seq` toIndexedStringForward newIndex (drop n bs))
                  
toIndexedStringBackward :: Point -> [Word8] -> [(Point,Char)]
toIndexedStringBackward curIdx bs = case UF8Codec.decode (reverse $ decodeBack bs) of
    Nothing -> []
    Just (c,n) -> let newIndex = curIdx - fromIntegral n in
                      (newIndex,c) : (newIndex `seq` toIndexedStringBackward newIndex (drop n bs))
    

decodeBack :: [Word8] -> [Word8]
decodeBack [] = []
decodeBack (x:xs)
    | x < 128 || x > 192  = [x] -- beginning of char: take it and stop
    | otherwise           = x : decodeBack xs    -- continue


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


getAst :: BufferImpl syntax -> syntax
getAst FBufferData {hlCache = HLState (SynHL {hlGetTree = gt}) cache} = gt cache



--------------------------------------------------------
-- DERIVES GENERATED CODE
-- DO NOT MODIFY BELOW THIS LINE
-- CHECKSUM: 1150468583

instance Binary MarkValue
    where put (MarkValue x1 x2) = return () >> (put x1 >> put x2)
          get = case 0 of
                    0 -> ap (ap (return MarkValue) get) get

instance Binary Update
    where put (Insert x1
                      x2
                      x3) = putWord8 0 >> (put x1 >> (put x2 >> put x3))
          put (Delete x1
                      x2
                      x3) = putWord8 1 >> (put x1 >> (put x2 >> put x3))
          get = getWord8 >>= (\tag_ -> case tag_ of
                                           0 -> ap (ap (ap (return Insert) get) get) get
                                           1 -> ap (ap (ap (return Delete) get) get) get)

instance Binary UIUpdate
    where put (TextUpdate x1) = putWord8 0 >> put x1
          put (StyleUpdate x1 x2) = putWord8 1 >> (put x1 >> put x2)
          get = getWord8 >>= (\tag_ -> case tag_ of
                                           0 -> ap (return TextUpdate) get
                                           1 -> ap (ap (return StyleUpdate) get) get)
