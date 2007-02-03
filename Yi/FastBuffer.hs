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

--
-- | A fast 'Buffer' implementation
--

-- NB buffers have no concept of multiwidth characters. There is an
-- assumption that a character has width 1, including tabs.

module Yi.FastBuffer (Point, Size, BufferImpl, newBI, deleteNAtI, moveToI, insertNI, pointBI, nelemsBI, finaliseBI, sizeBI, writeBI, curLnI, gotoLnI, searchBI, regexBI, getMarkBI, setMarkBI, unsetMarkBI, nelemsBIH) where

import Yi.Regex
import Yi.Debug

import Yi.Syntax
import Yi.Syntax.Haskell

import qualified Data.Map as M
import Data.List (mapAccumL)
import Yi.Vty (Attr)

import Control.Concurrent.MVar
import Control.Exception        ( assert )

import Foreign.C.String
import Foreign.C.Types          ( CChar )
import Foreign.Marshal.Alloc    ( free )
import Foreign.Marshal.Array
import Foreign.Ptr              ( Ptr, nullPtr, minusPtr )
import Foreign.Storable         ( poke )

import qualified Data.ByteString.Char8 as B

-- ---------------------------------------------------------------------
--
-- | The buffer itself is stored as a
-- mutable byte array. 
--
-- Problems with this implementation:
-- * Does not support unicode
-- * Does not support very huge buffers
-- * Is not optimized
--
-- In the concurrent world, buffers are locked during use.
--

type Point = Int
type Size  = Int
type MarkKey = Int  -- 0: point, 1: mark
type MarkValue = (Int, Bool) -- (Point, leftBound)
type Marks = M.Map MarkKey MarkValue

type BufferImpl = MVar FBufferData

data FBufferData =
        FBufferData { _rawmem  :: !(Ptr CChar)     -- raw memory           (ToDo unicode)
                    , marks    :: !Marks
                    -- TODO: use weak refs as to automatically free unreferenced marks.
                    , _contsize :: !Int             -- length of contents
                    , _rawsize  :: !Int             -- raw size of buffer
                    }



--
-- | Resize an FBufferData
--
resizeFB_ :: FBufferData -> Int -> IO FBufferData
resizeFB_ (FBufferData ptr p e _) sz = do
    ptr' <- reallocArray0 ptr sz
    return (FBufferData ptr' p e sz)

--
-- | New FBuffer filled from string.
--
stringToFBuffer :: String -> IO FBufferData
stringToFBuffer s = do
    let size_i = length s
        r_size = size_i + 2048
    ptr <- mallocArray0 r_size
    pokeArray ptr (map castCharToCChar s) -- Unicode
    poke (ptr `advancePtr` size_i) (castCharToCChar '\0')
    return (FBufferData ptr (M.fromList [(0,(0,pointLeftBound)), (1,(0,markLeftBound))]) size_i r_size)


--
-- | read @n@ chars from buffer @b@, starting at @i@
--
readChars :: Ptr CChar -> Int -> Int -> IO [Char]
readChars p n i = do s <- peekArray n (p `advancePtr` i)
                     return $ map castCCharToChar s
{-# INLINE readChars #-}

--
-- | Write string into buffer.
--
writeChars :: Ptr CChar -> [Char] -> Int -> IO ()
writeChars p cs i = pokeArray (p `advancePtr` i) (map castCharToCChar cs)
{-# INLINE writeChars #-}

--
-- | Copy chars around the buffer.
--
shiftChars :: Ptr CChar -> Int -> Int -> Int -> IO ()
shiftChars ptr dst_off src_off len = do
    let dst = ptr `advancePtr` dst_off :: Ptr CChar
        src = ptr `advancePtr` src_off
    moveArray dst src len
    poke (dst `advancePtr` len) (castCharToCChar '\0')
{-# INLINE shiftChars #-}


------------------------------------------------------------------------

foreign import ccall unsafe "string.h strstr"
    cstrstr :: Ptr CChar -> Ptr CChar -> IO (Ptr CChar)

foreign import ccall unsafe "YiUtils.h countLines"
   ccountLines :: Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h findStartOfLineN"
   cfindStartOfLineN :: Ptr CChar -> Int -> Int -> Int -> IO Int

------------------------------------------------------------------------

-- May need to resize buffer. How do we append to eof?
insertN' :: FBufferData -> [Char] -> Int -> IO FBufferData
insertN' fb [] _ = return fb
insertN' fb@(FBufferData _ _ old_end old_max) cs cs_len = do
        let need_len = old_end + cs_len
        (FBufferData ptr pnts end mx) <-
            if need_len >= old_max then resizeFB_ fb (need_len + 2048)
                                   else return fb
        let (pnt,_) = pnts M.! 0
            len = max 0 (min (end - pnt) end) -- number of chars to shift
            dst = pnt + cs_len      -- point to start
            nend = dst + len        -- new length afterwards
        -- logPutStrLn $ "insertN' " ++ show cs ++ show pnt
        shiftChars ptr dst pnt len
        writeChars ptr cs pnt
        return (FBufferData ptr (shiftMarks pnt cs_len pnts) nend mx)
{-# INLINE insertN' #-}


shiftMarks :: Point -> Int -> Marks -> Marks
shiftMarks from by = M.map $ \(p, leftBound) -> (shift p leftBound, leftBound)
    where shift p leftBound | p < from  = p
                            | p == from = if leftBound then p else p'
                            | otherwise {- p > from -} = p'
                     where p' = max from (p + by)

------------------------------------------------------------------------

deleteN' :: FBufferData -> Int -> Int -> IO FBufferData
deleteN' b 0 _ = return b
deleteN' (FBufferData ptr pnts end mx) n pos = do
        let src = inBounds (pos + n) end     -- start shifting back from
            len = inBounds (end-pos-n) end   -- length of shift
            end'= pos + len                  -- new end
        shiftChars ptr pos src len
        return (FBufferData ptr (shiftMarks pos (negate len) pnts) end' mx)
{-# INLINE deleteN' #-}

------------------------------------------------------------------------
--
-- | 'FBuffer' is a member of the 'Buffer' class, providing fast
-- indexing operations. It is implemented in terms of a mutable byte
-- array.
--

--instance Buffer FBufferData where

-- | Construct a new buffer initialised with the supplied text
newBI :: [Char] -> IO BufferImpl
newBI s = newMVar =<< stringToFBuffer s

-- | Free any resources associated with this buffer
finaliseBI :: BufferImpl -> IO ()
finaliseBI fb = withMVar fb $ \(FBufferData ptr _ _ _) -> free ptr

-- | Number of characters in the buffer
sizeBI      :: BufferImpl -> IO Int
sizeBI fb = withMVar fb $ \(FBufferData _ _ n _) -> return n

-- | Extract the current point
pointBI     :: BufferImpl -> IO Int
pointBI fb = withMVar fb $ \(FBufferData _ pnts e mx) -> do
    let (p,_) = (pnts M.! 0)
    assert ((p >= 0 && (p < e || e == 0)) && e <= mx) $ return p
{-# INLINE pointBI #-}


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI    :: BufferImpl -> Int -> Int -> IO [Char]
nelemsBI fb n i = withMVar fb $ \(FBufferData b _ e _) -> do
        let i' = inBounds i e
            n' = min (e-i') n
        readChars b n' i'

-- | Return @n@ elems starting at @i@ of the buffer as a list.
-- This routine also does syntax highlighting.
nelemsBIH    :: BufferImpl -> Int -> Int -> IO [(Char,Attr)]
nelemsBIH fb n i = do asStr <- withMVar fb $ \(FBufferData b _ e _) -> readChars b e 0
                      let (finst,colors_) = hlColorize highlighter (B.pack asStr) (hlStartState highlighter)
                          colors = colors_ ++ hlColorizeEOF highlighter finst
                      return (take n (drop i (zip asStr colors)))

------------------------------------------------------------------------
-- Point based editing

-- | Move point in buffer to the given index
moveToI     :: BufferImpl -> Int -> IO ()
moveToI fb i = modifyMVar_ fb $ \(FBufferData ptr pnts end mx) ->
    return $ FBufferData ptr (M.insert 0 (inBounds i end, pointLeftBound) pnts) end mx
{-# INLINE moveToI #-}


-- | Write an element into the buffer at the current point
writeBI :: BufferImpl -> Char -> IO ()
writeBI fb c = withMVar fb $ \(FBufferData ptr pnts _ _) -> do
        let off = fst (pnts M.! 0)
        writeChars ptr [c] off
{-# INLINE writeBI #-}

-- | Insert the list at current point, extending size of buffer
insertNI    :: BufferImpl -> [Char] -> IO ()
insertNI fb cs = modifyMVar_ fb $ \fb'-> insertN' fb' cs (length cs)

-- | @deleteNAt b n p@ deletes @n@ characters at position @p@
deleteNAtI :: BufferImpl -> Int -> Int -> IO ()
deleteNAtI fb n pos = modifyMVar_ fb $ \fb' -> deleteN' fb' n pos

------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLnI       :: BufferImpl -> IO Int
-- count number of \n from origin to point
curLnI fb = withMVar fb $ \(FBufferData ptr pnts _ _) -> ccountLines ptr 0 $ fst $ pnts M.! 0
{-# INLINE curLnI #-}

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLnI      :: BufferImpl -> Int -> IO Int
gotoLnI fb n = modifyMVar fb $ \(FBufferData ptr pnts e mx) -> do
        np <- cfindStartOfLineN ptr 0 e (n-1)       -- index from 0
        let fb' = FBufferData ptr (M.insert 0 (np,pointLeftBound) pnts) e mx
        n' <- if np > e - 1 -- if next line is end of file, then find out what line this is
              then return . subtract 1 =<< ccountLines ptr 0 np
              else return n         -- else it is this line
        return (fb', max 1 n')
{-# INLINE gotoLnI #-}

    ---------------------------------------------------------------------




-- | Return index of next string in buffer that matches argument
searchBI      :: BufferImpl -> [Char] -> IO (Maybe Int)
searchBI fb s = withMVar fb $ \(FBufferData ptr pnts _ _) -> 
        withCString s $ \str -> do
            p <- cstrstr (ptr `advancePtr` (fst $ pnts M.! 0)) str
            return $ if p == nullPtr then Nothing
                                     else Just (p `minusPtr` ptr)

-- | Return indices of next string in buffer matched by regex
regexBI       :: BufferImpl -> Regex -> IO (Maybe (Int,Int))
regexBI fb re = withMVar fb $ \(FBufferData ptr pnts _ _) -> do
        let p = (fst $ pnts M.! 0)
        mmatch <- regexec re ptr p
        case mmatch of
            Nothing        -> return Nothing
            Just ((i,j),_) -> return (Just (p+i,p+j))    -- offset from point


-- ------------------------------------------------------------------------
    ---------------------------------------------------------------------


{- 
   Okay if the mark is set then we return that, otherwise we
   return the point, which will mean that the calling function will
   see the selection area as null in length. 
-}
getMarkBI        :: BufferImpl -> IO Int
getMarkBI fb = withMVar fb $ \(FBufferData { marks = p } ) ->
  return $ fst $ M.findWithDefault (p M.! 0) 1 p
    -- We look up position 1 in the marks, the default value to return
    -- if position 1 is not set, is position 0, ie the point.


-- | Set this buffer mark (TODO: have a set of these (bookmarks, error list, etc.))
setMarkBI        :: BufferImpl -> Int -> IO ()
setMarkBI fb pos = modifyMVar_ fb $ \fb' -> return $ fb' {marks = (M.insert 1 (pos,markLeftBound) (marks fb'))}

{-
  We must allow the unsetting of this mark, this will have the property
  that the point will always be returned as the mark.
-}
unsetMarkBI      :: BufferImpl -> IO ()
unsetMarkBI fb = modifyMVar_ fb $ \fb'-> return $ fb' { marks = (M.delete 1 (marks fb')) }


pointLeftBound, markLeftBound :: Bool
pointLeftBound = False
markLeftBound = True

------------------------------------------------------------------------

-- | calculate whether a move is in bounds. 
-- Note that one can always move to 1 char past the end of the buffer.
inBounds :: Int -> Int -> Int
inBounds i end | i <= 0    = 0
               | i > end   = max 0 end
               | otherwise = i
{-# INLINE inBounds #-}


{-
-- | Create a mark associated with this buffer.
newMarkB :: FBuffer -> Point -> Bool -> IO MarkKey
newMarkB (FBuffer { rawbuf = mv }) point leftBound = do 
  modifyMVar mv $ \fb -> do
    let idx :: MarkKey
        idx = case M.maxView (marks fb) of
                   Nothing -> 0
                   Just (_,(x,_)) -> x + 1 -- FIXME: this risk overflowing
    return (fb {marks = M.insert idx (point, leftBound) (marks fb)}, idx)
-}
