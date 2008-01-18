-- Copyright (c) 2004-5, 7-8, 8, 8 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

-- | 'Buffer' implementation, wrapping ByteString.
module Yi.Buffer.Implementation
  ( Update     ( .. )
  , Point
  , Mark
  , Size
  , BufferImpl
  , moveToI
  , applyUpdateI
  , isValidUpdate
  , pointBI
  , nelemsBI
  , sizeBI
  , curLnI
  , newBI
  , gotoLnRelI
  , offsetFromSolBI
  , searchBI
  , searchBackwards
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
import Yi.Syntax.Table

import qualified Data.Map as M
import Yi.Style

import Control.Monad
import Control.Arrow (second)

import Text.Regex.Base
import Text.Regex.Posix

import qualified Yi.FingerString as F
import Yi.FingerString (FingerString)
import qualified Data.ByteString.Char8 as B

import Data.Array
import Data.Maybe


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
                    , _markNames :: !(M.Map String Mark)
                    , hlcache    :: !(Maybe HLState)       -- ^ syntax highlighting state
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
newBI s = FBufferData (F.fromString s) mks M.empty Nothing []
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
sizeBI (FBufferData p _ _ _ _) = F.length p

-- | Extract the current point
pointBI :: BufferImpl -> Int
pointBI (FBufferData _ mks _ _ _) = markPosition (mks M.! pointMark)
{-# INLINE pointBI #-}

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI :: Int -> Int -> BufferImpl -> String
nelemsBI n i (FBufferData b _ _ _ _) =
        let i' = inBounds i (F.length b)
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
nelemsBIH n i fb = concat $ helper $ styleRangesBI n i fb
  where
    helper ((l,a):xs@((h,_):_)) = (fmap (flip (,) a) (nelemsBI (h-l) l fb)) : helper xs
    helper _                    = []

-- | Return style information for the range of @n@ characters starting
--   at @i@. Style information is derived from syntax highlighting and
--   active overlays.
--   The returned list contains tuples (@p@,@s@) where every tuple is to
--   be interpreted as apply the style @s@ until position @p@ in the buffer.
--   In the final element @p@ = @n@ + @i@.
styleRangesBI :: Int -> Int -> BufferImpl -> [(Int, Style)]
styleRangesBI n i fb = fun fb
  where
    -- The first case is to handle when no 'Highlighter a' has
    -- been assigned to the buffer (via eg 'setSyntaxBI bi "haskell"')
    fun bd@(FBufferData b _ _ Nothing _) =
           let e = F.length b
               i' = inBounds i e
               n' = min (e-i') n
               cas = [(0, defaultStyle),(e, defaultStyle)]
           in cutRanges n' i' (overlay bd cas)
    -- in this, second, case 'hl' will be bound to a 'Highlighter a'
    -- eg Yi.Syntax.Haskell.highlighter (see Yi.Syntax for defn of Highlighter) which
    -- uses '(Data.ByteString.Char8.ByteString, Int)' as its parameterized state
    fun bd@(FBufferData b _ _ (Just (HLState hl)) _) =

      let (finst,colors_) = hlColorize hl b (hlStartState hl)
          colors = colors_ ++ hlColorizeEOF hl finst
      in cutRanges n i (overlay bd (makeRanges 0 colors))

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
    cutRanges m j =
      (++ [(j+m, defaultStyle)]) . (fst . splitRanges m) . (snd . splitRanges j)

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
moveToI i (FBufferData ptr mks nms hl ov) =
                 FBufferData ptr (M.insert pointMark (MarkValue (inBounds i end) pointLeftBound) mks) nms hl ov
    where end = F.length ptr
{-# INLINE moveToI #-}

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl -> Bool
isValidUpdate u b = case u of
                    (Delete p n)   -> check p && check (p + n)
                    (Insert p _)   -> check p
    where check x = x >= 0 && x <= F.length (mem b)


-- | Apply a /valid/ update
applyUpdateI :: Update -> BufferImpl -> BufferImpl
applyUpdateI u (FBufferData p mks nms hl ov) = FBufferData p' (M.map shift mks) nms hl (map (mapOvlMarks shift) ov)
    where (p', amount) = case u of
                           Insert pnt cs  -> (insertChars p (F.fromString cs) pnt, length cs)
                           Delete pnt len -> (deleteChars p pnt len, negate len)
          shift = shiftMarkValue (updatePoint u) amount
          -- FIXME: remove collapsed overlays

------------------------------------------------------------------------
-- Line based editing

curLnI :: BufferImpl -> Int
curLnI fb@(FBufferData ptr _ _ _ _) = 1 + F.count '\n' (F.take (pointBI fb) ptr)

-- | Go to line number @n@, relatively from this line. @0@ will go to
-- the start of this line. Returns the actual line difference we went
-- to (which may be not be the requested one, if it was out of range)
gotoLnRelI :: Int -> BufferImpl -> (BufferImpl, Int)
gotoLnRelI n fb = (moveToI np fb, max 1 n')
    where
     s = mem fb
     point = pointBI fb
     (n', np) = if n <= 0
      then
        let lineStarts = map (+1) ((F.elemIndicesEnd '\n') (F.take point s)) ++ [0]
            findLine acc _ [x]    = (acc, x)
            findLine acc 0 (x:_)  = (acc, x)
            findLine acc l (_:xs) = findLine (acc - 1) (l + 1) xs
            findLine _ _ []       =
              error "lineStarts ends with 0 : ... this cannot happen"
        in findLine 0 n lineStarts
      else
        let lineStarts = map (+1) ((F.elemIndices '\n') (F.drop point s))
            findLine acc _ []     = (acc, 0) -- try to go forward, but there is no such line.
            findLine acc _ [x]    = (acc + 1, x)
            findLine acc 1 (x:_)  = (acc, x)
            findLine acc l (_:xs) = findLine (acc + 1) (l - 1) xs
        in second (point +) (findLine 0 n lineStarts)

-- | Return index of next string in buffer that matches argument
searchBI :: String -> BufferImpl -> Maybe Int
searchBI s fb@(FBufferData ptr _ _ _ _) = fmap (+ pnt) $ F.findSubstring (B.pack s) $ F.drop pnt ptr
    where pnt = pointBI fb

-- | Return index of previous string in buffer that matches argument
searchBackwards :: String -> BufferImpl -> Maybe Int
searchBackwards s fb@(FBufferData ptr _ _ _ _) = (listToMaybe . reverse) results
    where results :: [Int]
          results = F.findSubstrings (B.pack s) $ F.take (pnt + length s) ptr
          pnt :: Int
          pnt = pointBI fb -- pnt == current point

offsetFromSolBI :: BufferImpl -> Int
offsetFromSolBI fb@(FBufferData ptr _ _ _ _) = pnt - maybe 0 (1 +) (F.elemIndexEnd '\n' (F.take pnt ptr))
    where pnt = pointBI fb


-- | Return indices of next string in buffer matched by regex
regexBI :: Regex -> BufferImpl -> Maybe (Int,Int)
regexBI re fb@(FBufferData ptr _ _ _ _) =
    let p = pointBI fb
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

-- Basically this function just takes the name of a 'Highlighter a' (from Yi.Syntax.Table)
-- and sets that to be the current highlighter for this buffer.
setSyntaxBI :: String -> BufferImpl -> BufferImpl
setSyntaxBI sy fb = case highlighters M.! sy of
                      ExtHL e -> fb { hlcache = HLState `fmap` e }

pointLeftBound, markLeftBound :: Bool
pointLeftBound = False
markLeftBound = True

------------------------------------------------------------------------

-- | Returns the requested mark, creating a new mark with that name (at point) if needed
getMarkBI :: Maybe String -> BufferImpl -> (BufferImpl, Mark)
getMarkBI name b = getMarkDefaultPosBI name (pointBI b) b

-- | Returns the requested mark, creating a new mark with that name (at the supplied position) if needed
getMarkDefaultPosBI :: Maybe String -> Int -> BufferImpl -> (BufferImpl, Mark)
getMarkDefaultPosBI name defaultPos fb@(FBufferData ptr mks nms hl ov) =
  case flip M.lookup nms =<< name of
    Just m' -> (fb, m')
    Nothing ->
           let newMark = Mark (1 + max 1 (markId $ fst (M.findMax mks)))
               nms' = case name of
                        Nothing -> nms
                        Just nm -> M.insert nm newMark nms
               mks' = M.insert newMark (MarkValue defaultPos False) mks
           in (FBufferData ptr mks' nms' hl ov, newMark)


