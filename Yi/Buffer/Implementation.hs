{-# LANGUAGE PatternGuards, ExistentialQuantification #-}

-- Copyright (c) 2004-5, 7-8 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- | 'Buffer' implementation, wrapping ByteString.
module Yi.Buffer.Implementation
  ( Update     ( .. )
  , Point
  , Mark
  , Size
  , Direction (..)
  , BufferImpl
  , moveToI
  , applyUpdateI
  , isValidUpdate
  , applyUpdateWithMoveI
  , reverseUpdateI
  , pointBI
  , nelemsBI
  , sizeBI
  , curLnI
  , newBI
  , gotoLnRelI
  , offsetFromSolBI
  , searchBI
  , regexBI
  , getMarkBI
  , getMarkPointBI
  , setMarkPointBI
  , unsetMarkBI
  , getSelectionMarkBI
  , nelemsBIH
  , styleRangesBI
  , setSyntaxBI
  , addOverlayBI
  , inBounds
)
where

import Yi.Syntax

import qualified Data.Map as M
import Yi.Style

import Control.Monad

import Text.Regex.Base
import Text.Regex.Posix

import qualified Yi.FingerString as F
import Yi.FingerString (FingerString)
import qualified Data.ByteString.Char8 as B

import Data.Array
import Data.Maybe


-- | Direction of movement inside a buffer
data Direction = Backward
               | Forward
                 deriving Eq

type Point = Int
type Size  = Int

newtype Mark = Mark {markId::Int} deriving (Eq, Ord, Show)
pointMark, markMark :: Mark
pointMark = Mark 0 -- 'point' - the insertion point mark
markMark = Mark 1 -- 'mark' - the selection mark

data MarkValue = MarkValue {markPosition::Int, _markIsLeftBound::Bool}
               deriving (Eq, Show)

type Marks = M.Map Mark MarkValue

type BufferImpl = FBufferData

data HLState = forall a. Eq a => HLState !(Highlighter a)

-- ---------------------------------------------------------------------
--
-- | The buffer text itself is stored as ByteString.
--
-- Problems with this implementation:
-- * Does not support unicode
-- * Is not optimized (O(n) operations)
--

data FBufferData =
        FBufferData { mem        :: !FingerString          -- ^ buffer text
                    , marks      :: !Marks                 -- ^ Marks for this buffer
                    , markNames  :: !(M.Map String Mark)
                    , hlCache    :: !HLState       -- ^ syntax highlighting state
--                    , hlResult   :: [(Int,Style)] -- ^ note: Lazy component!
                    , overlays   :: ![(MarkValue, MarkValue, Style)] -- ^ list of visual overlay regions
                    -- Overlays should not use Mark, but directly Point
                    }


--
-- | Mutation actions (from the undo or redo list)
--
-- We use the /partial checkpoint/ (Berlage, pg16) strategy to store
-- just the components of the state that change.
--

data Update = Insert {updatePoint :: !Point, insertUpdateString :: !String} -- FIXME: use ByteString
            | Delete {updatePoint :: !Point, deleteUpdateSize :: !Size}
              deriving Show


--------------------------------------------------
-- Low-level primitives.

-- | New FBuffer filled from string.
newBI :: String -> FBufferData
newBI s = FBufferData (F.fromString s) mks M.empty (HLState noHighlighter) []
    where
    mks = M.fromList [(pointMark, MarkValue 0 pointLeftBound)]

-- | read @n@ chars from buffer @b@, starting at @i@
readChars :: FingerString -> Int -> Int -> FingerString
readChars p n i = F.take n $ F.drop i $ p
{-# INLINE readChars #-}

-- | Write string into buffer.
insertChars :: FingerString -> FingerString -> Int -> FingerString
insertChars p cs i = left `F.append` cs `F.append` right
    where (left,right) = F.splitAt i p
{-# INLINE insertChars #-}


-- | Write string into buffer.
deleteChars :: FingerString -> Int -> Int -> FingerString
deleteChars p i n = left `F.append` right
    where (left,rest) = F.splitAt i p
          right = F.drop n rest
{-# INLINE deleteChars #-}

-- | calculate whether a move is in bounds.
-- Note that one can move to 1 char past the end of the buffer.
inBounds :: Int -> Int -> Int
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
              where p' = max from (p + by)


mapOvlMarks :: (a -> b) -> (a, a, v) -> (b, b, v)
mapOvlMarks f (s,e,v) = (f s, f e, v)

-------------------------------------
-- * "high-level" (exported) operations

-- | Number of characters in the buffer
sizeBI :: BufferImpl -> Int
sizeBI fb = F.length $ mem fb

-- | Extract the current point
pointBI :: BufferImpl -> Int
pointBI fb = markPosition ((marks fb) M.! pointMark)
{-# INLINE pointBI #-}

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI :: Int -> Int -> BufferImpl -> String
nelemsBI n i fb =
        let b = mem fb
            i' = inBounds i (F.length b)
            n' = min (F.length b - i') n
        in F.toString $ readChars b n' i'


-- | Add a style "overlay" between the given points.
addOverlayBI :: Point -> Point -> Style -> BufferImpl -> BufferImpl
addOverlayBI s e sty fb =
    let sm = MarkValue s True
        em = MarkValue e False
    in fb{overlays=(sm,em,sty) : overlays fb}

-- | Return @n@ elems starting at @i@ of the buffer as a list.
-- This routine also does syntax highlighting and applies overlays.
nelemsBIH :: Int -> Int -> BufferImpl -> [(Char,Style)]
nelemsBIH n i fb = helper i defaultStyle (styleRangesBI n i fb) (nelemsBI n i fb)
  where
    helper _   sty [] cs = setSty sty cs
    helper pos sty ((end,sty'):xs) cs = setSty sty left ++ helper end sty' xs right
        where (left, right) = splitAt (end - pos) cs
    setSty sty cs = [(c,sty) | c <- cs]

-- | Return style information for the range of @n@ characters starting
--   at @i@. Style information is derived from syntax highlighting and
--   active overlays.
--   The returned list contains tuples (@p@,@s@) where every tuple is to
--   be interpreted as apply the style @s@ from position @p@ in the buffer.
--   In the final element @p@ = @n@ + @i@.
styleRangesBI :: Int -> Int -> BufferImpl -> [(Int, Style)]
styleRangesBI n i fb@FBufferData {hlCache = HLState hl} = fun
  where
    fun =
      let b = mem fb
          colors = hlColorizeEOF hl 
                   (hlColorize hl (F.toLazyByteString b) (hlStartState hl))
      in cutRanges n i (overlay fb (makeRanges 0 colors))


    -- The parser produces a list of token sizes, convert them to buffer indices
    makeRanges :: Int -> [(Int,Style)] -> [(Int, Style)]
    makeRanges o [] = [(o,defaultStyle)]
    makeRanges o ((m,c):cs) = (o, c):makeRanges (o + m) cs

    -- Split the range list so that all split points less then x
    -- is in the left and all greater or equal in the right.
    -- Insert a new switch at x if there is none. If the new
    -- switch is left of existing switches, use a as default attribute
    splitRangesDefault :: a -> Int -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
    splitRangesDefault a x [] = ([], [(x,a)])
    splitRangesDefault a x ((y,b):ys) =
      case x `compare` y of
        LT -> ([], (x,a):(y,b):ys)
        EQ -> ([], (y,b):ys)
        GT -> let (ls, rs) = splitRangesDefault b x ys in ((y,b):ls, rs)

    splitRanges :: Int -> [(Int, Style)] -> ([(Int, Style)], [(Int, Style)])
    splitRanges = splitRangesDefault defaultStyle

    cutRanges :: Int -> Int -> [(Int, Style)] -> [(Int, Style)]
    cutRanges m j = takeRanges (j+m) . snd . splitRanges j
      where takeRanges k xs = let (ls, r:_) = splitRanges k xs in ls ++ [r]

    overlayRanges :: Int -> Int -> Style -> [(Int, Style)] -> [(Int, Style)]
    overlayRanges l h a rs = left ++ adjusted ++ right
      where
        (left, rest)    = splitRanges l rs
        (center, right) = splitRanges h rest
        adjusted        = fmap (\(m,b) -> (m, attrOver a b)) center


    overlay :: FBufferData -> [(Int, Style)] -> [(Int, Style)]
    overlay bd rs =
      foldr (\(sm, em, a) -> overlayRanges (markPosition sm) (markPosition em) a) rs (overlays bd)

    --attrOver att1 att2 = att1 .|. (att2 .&. 0xFFFF0000) -- Overwrite colors, keep attrs (bold, underline etc)
    attrOver att1 _att2 = att1 -- Until Vty exposes interface for attr merging....


------------------------------------------------------------------------
-- Point based editing

-- | Move point in buffer to the given index
moveToI :: Int -> BufferImpl -> BufferImpl
moveToI i fb = fb {marks = M.insert pointMark (MarkValue (inBounds i end) pointLeftBound) $ marks fb}
    where end = F.length (mem fb)
{-# INLINE moveToI #-}

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl -> Bool
isValidUpdate u b = case u of
                    (Delete p n)   -> check p && check (p + n)
                    (Insert p _)   -> check p
    where check x = x >= 0 && x <= F.length (mem b)


-- | Apply a /valid/ update
applyUpdateI :: Update -> BufferImpl -> BufferImpl
applyUpdateI u fb = fb {mem = p', marks = M.map shift (marks fb),
                        overlays = map (mapOvlMarks shift) (overlays fb)}
    where (p', amount) = case u of
                           Insert pnt cs  -> (insertChars p (F.fromString cs) pnt, length cs)
                           Delete pnt len -> (deleteChars p pnt len, negate len)
          shift = shiftMarkValue (updatePoint u) amount
          p = mem fb
          -- FIXME: remove collapsed overlays
   
-- | Apply a /valid/ update and also move point in buffer to update position
applyUpdateWithMoveI :: Update -> BufferImpl -> BufferImpl
applyUpdateWithMoveI u b = applyUpdateI u (moveToI (updatePoint u) b)

-- | Reverse the given update
reverseUpdateI :: Update -> BufferImpl -> Update
reverseUpdateI (Delete p n)  b = Insert p (nelemsBI n p b)
reverseUpdateI (Insert p cs) _ = Delete p (length cs)


------------------------------------------------------------------------
-- Line based editing

curLnI :: BufferImpl -> Int
curLnI fb = 1 + F.count '\n' (F.take (pointBI fb) (mem fb))


-- | Go to line number @n@, relatively from this line. @0@ will go to
-- the start of this line. Returns the actual line difference we went
-- to (which may be not be the requested one, if it was out of range)
-- Note that the line-difference returned will be negative if we are
-- going backwards to previous lines (that is if @n@ was negative).
-- Also note: it's legal to do a @gotoLnRelI 0@ this will move to
-- the start of the current line, which maybe what was required.
gotoLnRelI :: Int -> BufferImpl -> (BufferImpl, Int)
gotoLnRelI n fb = 
  (moveToI newPoint fb, difference)
  where
  -- The text of the buffer
  s     = mem fb
  -- The current point that we are at in the buffer.
  point = pointBI fb
  (difference, newPoint)
    -- Important that we go up if it is 0 since findDownLine
    -- fails for zero.
    | n <= 0    = findUpLine 0 n upLineStarts
    | otherwise = findDownLine 1 n downLineStarts

  -- The offsets of all line beginnings above the current point.
  upLineStarts = map (+1) ((F.elemIndicesEnd '\n') (F.take point s)) ++ [0]

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
  downLineStarts = map (+(1 + point)) ((F.elemIndices '\n') (F.drop point s))

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
searchBI :: Direction -> String -> BufferImpl -> Maybe Int
searchBI dir s fb = case dir of
      Forward -> fmap (+ pnt) $ F.findSubstring (B.pack s) $ F.drop pnt ptr
      Backward -> listToMaybe $ reverse $ F.findSubstrings (B.pack s) $ F.take (pnt + length s) ptr
    where pnt = pointBI fb -- pnt == current point
          ptr = mem fb

offsetFromSolBI :: BufferImpl -> Int
offsetFromSolBI fb = pnt - maybe 0 (1 +) (F.elemIndexEnd '\n' (F.take pnt ptr))
    where pnt = pointBI fb
          ptr = mem fb


-- | Return indices of next string in buffer matched by regex
regexBI :: Regex -> BufferImpl -> Maybe (Int,Int)
regexBI re fb = 
    let p = pointBI fb
        ptr = mem fb
        mmatch = matchOnce re (F.toByteString $ F.drop p ptr)
    in case mmatch of
         Just arr | ((off,len):_) <- elems arr -> Just (p+off,p+off+len)
         _ -> Nothing

getSelectionMarkBI :: BufferImpl -> Mark
getSelectionMarkBI _ = markMark -- FIXME: simplify this.

-- | Returns ths position of the 'point' mark if the requested mark is unknown (or unset)
getMarkPointBI :: Mark -> BufferImpl -> Point
getMarkPointBI m fb = markPosition (getMark fb m)

getMark :: BufferImpl -> Mark -> MarkValue
getMark (FBufferData { marks = marksMap } ) m = M.findWithDefault (marksMap M.! pointMark) m marksMap
                 -- We look up mark m in the marks, the default value to return
                 -- if mark m is not set, is the pointMark

-- | Set a mark point
setMarkPointBI :: Mark -> Point -> BufferImpl -> BufferImpl
setMarkPointBI m pos fb = fb {marks = M.insert m (MarkValue pos (if m == markMark then markLeftBound else False)) (marks fb)}

{-
  We must allow the unsetting of this mark, this will have the property
  that the point will always be returned as the mark.
-}
unsetMarkBI :: BufferImpl -> BufferImpl
unsetMarkBI fb = fb { marks = (M.delete markMark (marks fb)) }

-- Formerly the highlighters table was directly used
-- 'Yi.Syntax.Table.highlighters'. However avoiding to depends on all
-- highlighters implementation speed up compilation a lot when working on a
-- syntax highlighter.
setSyntaxBI :: ExtHL -> BufferImpl -> BufferImpl
setSyntaxBI (ExtHL e) fb = -- updateHl 0 $
                           fb { hlCache = HLState e }


pointLeftBound, markLeftBound :: Bool
pointLeftBound = False
markLeftBound = True

------------------------------------------------------------------------

-- | Returns the requested mark, creating a new mark with that name (at point) if needed
getMarkBI :: Maybe String -> BufferImpl -> (BufferImpl, Mark)
getMarkBI name b = getMarkDefaultPosBI name (pointBI b) b

-- | Returns the requested mark, creating a new mark with that name (at the supplied position) if needed
getMarkDefaultPosBI :: Maybe String -> Int -> BufferImpl -> (BufferImpl, Mark)
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


