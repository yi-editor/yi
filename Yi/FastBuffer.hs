--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

-- | 'Buffer' implementation, wrapping bytestring.

module Yi.FastBuffer (Update(..), Point, Mark, Size, BufferImpl, moveToI, applyUpdateI, isValidUpdate,
                      pointBI, nelemsBI, sizeBI, curLnI, newBI,
                      gotoLnI, searchBI, regexBI, 
                      getMarkBI, getMarkPointBI, setMarkPointBI, unsetMarkBI, getSelectionMarkBI,
                      nelemsBIH, setSyntaxBI, addOverlayBI,
                      inBounds) where

import Yi.Debug
import Yi.Syntax
import Yi.Syntax.Table

import qualified Data.Map as M
import Yi.Style

import Control.Monad

import Text.Regex.Base
import Text.Regex.Posix

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base 

import Data.Array


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
        FBufferData { mem        :: !ByteString            -- ^ buffer text
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
newBI s = FBufferData (B.pack s) mks M.empty Nothing []
    where
    mks = M.fromList [(pointMark, MarkValue 0 pointLeftBound)]

-- | read @n@ chars from buffer @b@, starting at @i@
readChars :: ByteString -> Int -> Int -> ByteString
readChars p n i = B.take n $ B.drop i $ p
{-# INLINE readChars #-}

-- | Write string into buffer.
insertChars :: ByteString -> ByteString -> Int -> ByteString
insertChars p cs i = left `B.append` cs `B.append` right
    where (left,right) = B.splitAt i p
{-# INLINE insertChars #-}


-- | Write string into buffer.
deleteChars :: ByteString -> Int -> Int -> ByteString
deleteChars p i n = left `B.append` right
    where (left,rest) = B.splitAt i p
          right = B.drop n rest
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
sizeBI (FBufferData p _ _ _ _) = B.length p

-- | Extract the current point
pointBI :: BufferImpl -> Int
pointBI (FBufferData _ mks _ _ _) = markPosition (mks M.! pointMark)
{-# INLINE pointBI #-}


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI :: Int -> Int -> BufferImpl -> [Char]
nelemsBI n i (FBufferData b _ _ _ _) = 
        let i' = inBounds i (B.length b)
            n' = min (B.length b - i') n
        in B.unpack $ readChars b n' i'


-- | Add a style "overlay" between the given points.
addOverlayBI :: Point -> Point -> Style -> BufferImpl -> BufferImpl
addOverlayBI s e sty fb = 
    let sm = MarkValue s True
        em = MarkValue e False
    in fb{overlays=(sm,em,sty) : overlays fb}

-- | Return @n@ elems starting at @i@ of the buffer as a list.
-- This routine also does syntax highlighting and applies overlays.
nelemsBIH :: Int -> Int -> BufferImpl -> [(Char,Style)]
nelemsBIH n i fb = fun fb
    where
      -- The first case is to handle when no 'Highlighter a' has
      -- been assigned to the buffer (via eg 'setSyntaxBI bi "haskell"')
      fun bd@(FBufferData b _ _ Nothing _) =
             let e = B.length b
                 i' = inBounds i e
                 n' = min (e-i') n
                 cas = map (flip (,) defaultStyle) (B.unpack $ readChars b n' i')
             in overlay bd cas
      -- in this, second, case 'hl' will be bound to a 'Highlighter a'
      -- eg Yi.Syntax.Haskell.highlighter (see Yi.Syntax for defn of Highlighter) which
      -- uses '(Data.ByteString.Char8.ByteString, Int)' as its parameterized state
      fun bd@(FBufferData b _ _ (Just (HLState hl)) _) =
        
        let (finst,colors_) = hlColorize hl b (hlStartState hl)
            colors = colors_ ++ hlColorizeEOF hl finst
        in overlay bd (take n (drop i (zip (B.unpack b) colors)))

      overlay :: FBufferData -> [(Char,Style)] -> [(Char,Style)]
      overlay bd xs = map (\((c,a),j)->(c, ov bd (a,j))) (zip xs [i..])

      ov :: FBufferData -> (Style, Int) -> Style
      ov bd (sh_attr, ind) = foldr (\(sm, em, a) att ->
                                    if betweenMarks sm em ind
                                    then a `attrOver` att
                                    else att)
                             sh_attr (overlays bd)

      betweenMarks m1 m2 pos = pos >= markPosition m1 && pos < markPosition m2

      --attrOver att1 att2 = att1 .|. (att2 .&. 0xFFFF0000) -- Overwrite colors, keep attrs (bold, underline etc)
      attrOver att1 _att2 = att1 -- Until Vty exposes interface for attr merging....

------------------------------------------------------------------------
-- Point based editing

-- | Move point in buffer to the given index
moveToI :: Int -> BufferImpl -> BufferImpl
moveToI i (FBufferData ptr mks nms hl ov) =
                 FBufferData ptr (M.insert pointMark (MarkValue (inBounds i end) pointLeftBound) mks) nms hl ov
    where end = B.length ptr
{-# INLINE moveToI #-}

-- | Checks if an Update is valid
isValidUpdate :: Update -> BufferImpl -> Bool
isValidUpdate u b = case u of
                    (Delete p n) -> check p && check (p + n)
                    (Insert p _) -> check p
    where check x = x >= 0 && x <= B.length (mem b)


-- | Apply a /valid/ update 
applyUpdateI :: Update -> BufferImpl -> BufferImpl
applyUpdateI u (FBufferData p mks nms hl ov) = FBufferData p' (M.map shift mks) nms hl (map (mapOvlMarks shift) ov)
    where (p', amount) = case u of
                           Insert pnt cs  -> (insertChars p (B.pack cs) pnt, length cs)
                           Delete pnt len -> (deleteChars p pnt len, negate len)
          shift = shiftMarkValue (updatePoint u) amount
          -- FIXME: remove collapsed overlays 

------------------------------------------------------------------------
-- Line based editing

curLnI :: BufferImpl -> Int
curLnI fb@(FBufferData ptr _ _ _ _) = 1 + B.count '\n'  (B.take (pointBI fb) ptr)

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLnI :: Int -> BufferImpl -> (BufferImpl, Int)
gotoLnI n fb | n < 1 = (moveToI 0 fb, 1)
gotoLnI n fb = 
        let lineEnds = B.elemIndices '\n' (mem fb)
            lineStarts = 0 : map (+1) lineEnds

            findLine acc _ [x] = (acc, x)
            findLine acc 1 (x:_) = (acc, x)
            findLine acc l (_:xs) = findLine (acc + 1) (l - 1) xs

            (n', np) = findLine 1 n lineStarts
        in (moveToI np fb, max 1 n')

-- | Return index of next string in buffer that matches argument
searchBI :: [Char] -> BufferImpl -> Maybe Int
searchBI s fb@(FBufferData ptr _ _ _ _) = fmap (+ pnt) $ B.findSubstring (B.pack s) $ B.drop pnt ptr
    where pnt = pointBI fb

-- | Return indices of next string in buffer matched by regex
regexBI :: Regex -> BufferImpl -> Maybe (Int,Int)
regexBI re fb@(FBufferData ptr _ _ _ _) = 
    let p = pointBI fb
        mmatch = matchOnce re (B.drop p ptr)
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


