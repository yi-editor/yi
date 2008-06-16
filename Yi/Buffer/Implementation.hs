{-# LANGUAGE PatternGuards, ExistentialQuantification, DeriveDataTypeable, Rank2Types #-}

-- Copyright (c) 2004-5, 7-8 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | 'Buffer' implementation, wrapping ByteString.
module Yi.Buffer.Implementation
  ( UIUpdate (..)
  , Update (..)
  , Point
  , Mark
  , Size
  , Direction (..)
  , BufferImpl
  , Overlay
  , mkOverlay
  , overlayUpdate
  , moveToI
  , applyUpdateI
  , isValidUpdate
  , applyUpdateWithMoveI
  , reverseUpdateI
  , pointBI
  , nelemsBI
  , nelemsBI'
  , sizeBI
  , curLnI
  , newBI
  , gotoLnRelI
--  , offsetFromSolBI
  , charsFromSolBI
  , searchBI
  , regexBI
  , getMarkBI
  , getMarkPointBI
  , setMarkPointBI
  , unsetMarkBI
  , getSelectionMarkBI
  , setSyntaxBI
  , addOverlayBI
  , delOverlayBI
  , inBounds
  , findNextChar
  , updateSyntax
  , getAst
  , strokesRangesBI
)
where

import Yi.Prelude
import Prelude (take, takeWhile, dropWhile, map, length, reverse, zip)
import Yi.Syntax 

import qualified Data.Map as M
import Yi.Style

import Control.Monad

import Text.Regex.Base
import Text.Regex.Posix

import qualified Yi.FingerString as F
import Yi.FingerString (FingerString)
import qualified Data.ByteString as B
import Codec.Binary.UTF8.String as UTF8Codec
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

import Data.Array
import Data.Char
import Data.Maybe
import Data.Word
import qualified Data.Set as Set
import Yi.Debug
import Data.Typeable

-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving (Eq, Typeable)

newtype Mark = Mark {markId::Int} deriving (Eq, Ord, Show)
pointMark, markMark :: Mark
pointMark = Mark 0 -- 'point' - the insertion point mark
markMark = Mark 1 -- 'mark' - the selection mark

data MarkValue = MarkValue {markPosition::Point, _markIsLeftBound::Bool}
               deriving (Ord, Eq, Show)

type Marks = M.Map Mark MarkValue

type BufferImpl = FBufferData

data HLState syntax = forall cache. HLState !(Highlighter' cache syntax) cache

data Overlay = Overlay MarkValue MarkValue Style
               deriving (Ord, Eq)

data FBufferData syntax =
        FBufferData { mem        :: !FingerString          -- ^ buffer text
                    , marks      :: !Marks                 -- ^ Marks for this buffer
                    , markNames  :: !(M.Map String Mark)
                    , hlCache    :: !(HLState syntax)       -- ^ syntax highlighting state
                    -- ^ syn hl result. Actual result is (reverse fst ++ snd)
                    , overlays   :: !(Set.Set Overlay) -- ^ set of (non overlapping) visual overlay regions
                    , dirtyOffset :: !Point -- ^ Lowest modified offset since last recomputation of syntax 
                    }
        deriving Typeable

--
-- | Mutation actions (from the undo or redo list)
--
-- We use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--

data Update = Insert {updatePoint :: !Point, insertUpdateString :: !B.ByteString} 
            | Delete {updatePoint :: !Point, deleteUpdateSize :: !Size}
              deriving (Show, Typeable)

data UIUpdate = TextUpdate Update
              | StyleUpdate !Point !Size

--------------------------------------------------
-- Low-level primitives.

-- | New FBuffer filled from string.
newBI :: String -> FBufferData ()
newBI s = FBufferData (F.fromString $ UTF8Codec.encode s) mks M.empty (HLState noHighlighter (hlStartState noHighlighter)) Set.empty 0
    where mks = M.fromList [(pointMark, MarkValue 0 pointLeftBound)]

-- | read @n@ chars from buffer @b@, starting at @i@
readChars :: FingerString -> Int -> Point -> String
readChars p n (Point i) = take n $ LazyUTF8.toString $ F.toLazyByteString $ F.drop i $ p
{-# INLINE readChars #-}

-- | read @n@ bytes from buffer @b@, starting at @i@
readChunk :: FingerString -> Size -> Point -> FingerString
readChunk p (Size n) (Point i) = F.take n $ F.drop i $ p

-- | read @n@ bytes from buffer @b@, starting at @i@
readBytes :: FingerString -> Size -> Point -> B.ByteString
readBytes p n i = F.toByteString $ readChunk p n i

-- | Write string into buffer.
insertChars :: FingerString -> FingerString -> Point -> FingerString
insertChars p cs (Point i) = left `F.append` cs `F.append` right
    where (left,right) = F.splitAt i p
{-# INLINE insertChars #-}


-- | Write string into buffer.
deleteChars :: FingerString -> Point -> Size -> FingerString
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

shiftMarkValue :: Point -> Size -> MarkValue -> MarkValue
shiftMarkValue from by (MarkValue p leftBound) = MarkValue shifted leftBound
    where shifted | p < from  = p
                  | p == from = if leftBound then p else p'
                  | otherwise {- p > from -} = p'
              where p' = max from (p +~ by)

mapOvlMarks :: (MarkValue -> MarkValue) -> Overlay -> Overlay
mapOvlMarks f (Overlay s e v) = Overlay (f s) (f e) v

-------------------------------------
-- * "high-level" (exported) operations

-- | Point of EOF
sizeBI :: BufferImpl syntax -> Point
sizeBI fb = Point $ F.length $ mem fb

-- | Extract the current point
pointBI :: BufferImpl syntax -> Point
pointBI fb = markPosition ((marks fb) M.! pointMark)
{-# INLINE pointBI #-}

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI :: Int -> Point -> BufferImpl syntax -> String
nelemsBI n i fb = readChars (mem fb) n i

nelemsBI' :: Size -> Point -> BufferImpl syntax -> String
nelemsBI' n i fb = LazyUTF8.toString $ F.toLazyByteString $ readChunk (mem fb) n i

-- | Create an "overlay" for the style @sty@ between points @s@ and @e@
mkOverlay :: Point -> Point -> Style -> Overlay
mkOverlay s e = Overlay (MarkValue s True) (MarkValue e False)

-- | Obtain a style-update for a specific overlay
overlayUpdate :: Overlay -> UIUpdate
overlayUpdate (Overlay (MarkValue s _) (MarkValue e _) _) = StyleUpdate s (e ~- s)

-- | Add a style "overlay" between the given points.
addOverlayBI :: Overlay -> BufferImpl syntax -> BufferImpl syntax
addOverlayBI ov fb = fb{overlays = Set.insert ov (overlays fb)}

-- | Remove a previously added "overlay"
delOverlayBI :: Overlay -> BufferImpl syntax -> BufferImpl syntax
delOverlayBI ov fb = fb{overlays = Set.delete ov (overlays fb)}


-- | Return style information for the range @(i,j)@ Style information
--   is derived from syntax highlighting and active overlays.  The
--   returned list contains tuples @(l,s,r)@ where every tuple is to
--   be interpreted as apply the style @s@ from position @l@ to @r@ in
--   the buffer.  In each list, the strokes are guaranteed to be
--   ordered and non-overlapping.  The lists of strokes are ordered by
--   increasing priority.
strokesRangesBI :: Point -> Point -> BufferImpl syntax -> [[Stroke]]
strokesRangesBI i j fb@FBufferData {hlCache = HLState hl cache} =  result
  where
    dropBefore = dropWhile (\(_l,_s,r) -> r <= i)
    takeIn  = takeWhile (\(l,_s,_r) -> l <= j)

    layer1 = hlGetStrokes hl point i j cache
    layer2 = map overlayStroke $ Set.toList $ overlays fb

    result = map (map clampStroke . takeIn . dropBefore) [layer2, layer1]
    overlayStroke (Overlay sm  em a) = (markPosition sm, a, markPosition em)
    point = pointBI fb
    clampStroke (l,x,r) = (max i l, x, min j r)

------------------------------------------------------------------------
-- Point based editing

-- | Move point in buffer to the given index
moveToI :: Point -> BufferImpl syntax -> BufferImpl syntax
moveToI i fb = fb {marks = M.insert pointMark (MarkValue (inBounds i (Point end)) pointLeftBound) $ marks fb}
    where end = F.length (mem fb)
{-# INLINE moveToI #-}

findNextChar :: Int -> Point -> BufferImpl syntax -> Point
findNextChar m (Point i) fb 
    | m == 0 = Point i
    | m < 0 = let s = F.toReverseString $ F.take i $ mem fb
                  result = countBytes s (-m)
              in Point (i - result)
    | otherwise =
    {-m > 0-} let s = F.toString $ F.drop i $ mem fb
                  result = countBytes0 s  m
              in Point (i + result)
                   

-- Count the number of bytes to skip @n@ UTF8 codepoints, forward
countBytes0 :: [Word8] -> Int -> Int
countBytes0 [] _ = 0
countBytes0 _  0 = 0
countBytes0 (x:xs) m 
    | x < 128   = 1 + countBytes0 xs (m-1) -- one-byte codepoint
    | x > 192   = 1 + countBytes1 xs  m    -- long code point; find the end of it.
    | otherwise = 1 + countBytes0 xs  m    -- continuation; should not happen. skip.

countBytes1 :: [Word8] -> Int -> Int
countBytes1 [] _ = 0
countBytes1 (x:xs) m 
    | x < 128 || x > 192  = countBytes0 (x:xs) (m-1) -- beginning, long codepoint has ended.
    | otherwise = 1 + countBytes1 xs  m              -- continuation


-- Count the number of bytes to skip @n@ UTF8 codepoints, backwards.
countBytes :: [Word8] -> Int -> Int
countBytes [] _ = 0
countBytes _  0 = 0
countBytes (x:xs) m 
    | x < 128 || x > 192  = 1 + countBytes xs (m-1) -- beginning of char, count one.
    | otherwise           = 1 + countBytes xs  m    -- continuation, just skip.

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl syntax -> Bool
isValidUpdate u b = case u of
                    (Delete p n)   -> check p && check (p +~ n)
                    (Insert p _)   -> check p
    where check (Point x) = x >= 0 && x <= F.length (mem b)


-- | Apply a /valid/ update
applyUpdateI :: Update -> BufferImpl syntax -> BufferImpl syntax
applyUpdateI u fb = touchSyntax (updatePoint u) $ 
                    fb {mem = p', marks = M.map shift (marks fb),
                                   overlays = Set.map (mapOvlMarks shift) (overlays fb)}
                                   -- FIXME: this is inefficient; find a way to use mapMonotonic
                                   -- (problem is that marks can have different gravities)
    where (p', amount) = case u of
                           Insert pnt cs  -> (insertChars p (F.fromByteString cs) pnt, Size (B.length cs))
                           Delete pnt len -> (deleteChars p pnt len, negate len)
          shift = shiftMarkValue (updatePoint u) amount
          p = mem fb
          -- FIXME: remove collapsed overlays
   
-- | Apply a /valid/ update and also move point in buffer to update position
applyUpdateWithMoveI :: Update -> BufferImpl syntax -> BufferImpl syntax
applyUpdateWithMoveI u = applyUpdateI u . moveToI (updatePoint u)

-- | Reverse the given update
reverseUpdateI :: Update -> BufferImpl syntax -> Update
reverseUpdateI (Delete p n)  b = Insert p (readBytes (mem b) n p)
reverseUpdateI (Insert p cs) _ = Delete p (Size $ B.length cs)


------------------------------------------------------------------------
-- Line based editing

ord' :: Char -> Word8
ord' = fromIntegral . ord

newLine :: Word8
newLine = ord' '\n'

curLnI :: BufferImpl syntax -> Int
curLnI fb = 1 + F.count newLine (F.take (fromPoint $ pointBI fb) (mem fb))


-- | Go to line number @n@, relatively from this line. @0@ will go to
-- the start of this line. Returns the actual line difference we went
-- to (which may be not be the requested one, if it was out of range)
-- Note that the line-difference returned will be negative if we are
-- going backwards to previous lines (that is if @n@ was negative).
-- Also note: it's legal to do a @gotoLnRelI 0@ this will move to
-- the start of the current line, which maybe what was required.
gotoLnRelI :: Int -> BufferImpl syntax -> (BufferImpl syntax, Int)
gotoLnRelI n fb = 
  (moveToI (Point newPoint) fb, difference)
  where
  -- The text of the buffer
  s     = mem fb
  -- The current point that we are at in the buffer.
  Point point = pointBI fb
  (difference, newPoint)
    -- Important that we go up if it is 0 since findDownLine
    -- fails for zero.
    | n <= 0    = findUpLine 0 n upLineStarts
    | otherwise = findDownLine 1 n downLineStarts

  -- The offsets of all line beginnings above the current point.
  upLineStarts = map (+1) ((F.elemIndicesEnd newLine) (F.take point s)) ++ [0]

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
  downLineStarts = map (+(1 + point)) ((F.elemIndices newLine) (F.drop point s))

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

-- | Return index of next string in buffer that matches argument
searchBI :: Direction -> String -> BufferImpl syntax -> Maybe Point
searchBI dir s fb = fmap Point $ case dir of
      Forward -> fmap (pnt +) $ F.findSubstring (UTF8.fromString s) $ F.drop pnt ptr
      Backward -> listToMaybe $ reverse $ F.findSubstrings (UTF8.fromString s) $ F.take (pnt + length s) ptr
    where Point pnt = pointBI fb -- pnt == current point
          ptr = mem fb

offsetToEolBI fb = Size $ case (F.elemIndices newLine) (F.drop point s) of
                     [] -> F.length s - point
                     (x:_) -> x
    where s = mem fb
          Point point = pointBI fb
                   

offsetFromSolBI :: BufferImpl syntax -> Size
offsetFromSolBI fb = Size (pnt - maybe 0 (1 +) (F.elemIndexEnd newLine (F.take pnt ptr)))
    where Point pnt = pointBI fb
          ptr = mem fb

charsFromSolBI :: BufferImpl syntax -> String
charsFromSolBI fb = UTF8.toString $ readBytes ptr (Size (pnt - sol)) (Point sol)
  where sol = maybe 0 (1 +) (F.elemIndexEnd newLine (F.take pnt ptr))
        Point pnt = pointBI fb
        ptr = mem fb


-- | Return indices of next string in buffer matched by regex
regexBI :: Regex -> forall syntax. BufferImpl syntax -> Maybe (Point,Point)
regexBI re fb = 
    let Point p = pointBI fb
        ptr = mem fb
        mmatch = matchOnce re (F.toByteString $ F.drop p ptr)
    in case mmatch of
         Just arr | ((off,len):_) <- elems arr -> Just (Point (p+off),Point (p+off+len))
         _ -> Nothing

getSelectionMarkBI :: BufferImpl syntax -> Mark
getSelectionMarkBI _ = markMark -- FIXME: simplify this.

-- | Returns ths position of the 'point' mark if the requested mark is unknown (or unset)
getMarkPointBI :: Mark -> forall syntax. BufferImpl syntax -> Point
getMarkPointBI m fb = markPosition (getMark fb m)

getMark :: BufferImpl syntax -> Mark -> MarkValue
getMark (FBufferData { marks = marksMap } ) m = M.findWithDefault (marksMap M.! pointMark) m marksMap
                 -- We look up mark m in the marks, the default value to return
                 -- if mark m is not set, is the pointMark

-- | Set a mark point
setMarkPointBI :: Mark -> Point -> (forall syntax. BufferImpl syntax -> BufferImpl syntax)
setMarkPointBI m pos fb = fb {marks = M.insert m (MarkValue pos (if m == markMark then markLeftBound else False)) (marks fb)}

{-
  We must allow the unsetting of this mark, this will have the property
  that the point will always be returned as the mark.
-}
unsetMarkBI :: BufferImpl syntax -> BufferImpl syntax
unsetMarkBI fb = fb { marks = (M.delete markMark (marks fb)) }

-- Formerly the highlighters table was directly used
-- 'Yi.Syntax.Table.highlighters'. However avoiding to depends on all
-- highlighters implementation speeds up compilation a lot when working on a
-- syntax highlighter.
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
    where getText = Scanner 0 (\idx -> zip [idx..] (UTF8Codec.decode $ F.toString (F.drop (fromPoint idx) (mem fb))))
                                -- FIXME: UTF8: the indices are completely wrong!

pointLeftBound, markLeftBound :: Bool
pointLeftBound = False
markLeftBound = True

------------------------------------------------------------------------

-- | Returns the requested mark, creating a new mark with that name (at point) if needed
getMarkBI :: Maybe String -> BufferImpl syntax -> (BufferImpl syntax, Mark)
getMarkBI name b = getMarkDefaultPosBI name (pointBI b) b

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
               mks' = M.insert newMark (MarkValue defaultPos False) mks
           in (fb {marks = mks', markNames = nms'}, newMark)


getAst :: BufferImpl syntax -> syntax
getAst FBufferData {hlCache = HLState (SynHL {hlGetTree = gt}) cache} = gt cache
