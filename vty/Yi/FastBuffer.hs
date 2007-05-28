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

module Yi.FastBuffer (Point, Mark, Size, BufferImpl, newBI, deleteNAtI, moveToI, insertNI, 
                      pointBI, nelemsBI, finaliseBI, sizeBI, writeBI, curLnI, 
                      gotoLnI, searchBI, regexBI, 
                      getMarkBI, getMarkPointBI, setMarkPointBI, unsetMarkBI, getSelectionMarkBI,
                      nelemsBIH, setSyntaxBI, addOverlayBI) where

import Yi.Debug
import Yi.Cbits
import Yi.Syntax
import Yi.Syntax.Table

import qualified Data.Map as M
import Yi.Vty (Attr, attr, styleToAttr)

import Control.Concurrent.MVar
import Control.Exception        ( evaluate, assert )
import Control.Monad

import Foreign.C.String
import Foreign.C.Types          ( CChar )
import Foreign.Marshal.Alloc    ( free )
import Foreign.Marshal.Array
import Foreign.Ptr              ( Ptr, nullPtr, minusPtr, plusPtr )
import Foreign.Storable         ( poke )

import Text.Regex.Posix.Wrap

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

newtype Mark = Mark {markId::Int} deriving (Eq, Ord, Show)
pointMark, markMark :: Mark
pointMark = Mark 0 -- 'point' - the insertion point mark
markMark = Mark 1 -- 'mark' - the selection mark

data MarkValue = MarkValue {markPosition::Int, _markIsLeftBound::Bool}
               deriving (Eq, Show)

type Marks = M.Map Mark MarkValue

type BufferImpl = MVar FBufferData

data HLState = forall a. Eq a => HLState !(Highlighter a)

data FBufferData =
        FBufferData { _rawmem  :: !(Ptr CChar)     -- ^ raw memory           (ToDo unicode)
                    , marks    :: !Marks           -- ^ Marks for this buffer
                    -- TODO: use weak refs as to automatically free unreferenced marks.
                    , _markNames :: !(M.Map String Mark)
                    , _contsize :: !Int             -- ^ length of contents
                    , _rawsize  :: !Int             -- ^ raw size of buffer
                    , hlcache   :: !(Maybe HLState) -- ^ syntax highlighting state
                    , overlays  :: ![(Mark, Mark, Attr)] -- ^ list of visual overlay regions
                    }



--
-- | Resize an FBufferData
--
resizeFB_ :: FBufferData -> Int -> IO FBufferData
resizeFB_ (FBufferData ptr p m e _ hl ov) sz = do
    ptr' <- reallocArray0 ptr sz
    return (FBufferData ptr' p m e sz hl ov)

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
    return (FBufferData ptr mks M.empty size_i r_size Nothing [])
    where
    mks = M.fromList [(pointMark, MarkValue 0 pointLeftBound)
                      , (markMark, MarkValue 0 markLeftBound)
                     ]

addOverlayBI :: BufferImpl -> Point -> Point -> Attr -> IO ()
addOverlayBI fb s e a = do
                        sm <- getMarkDefaultPosBI fb Nothing s
                        em <- getMarkDefaultPosBI fb Nothing e
                        modifyMVar_ fb $ \bd -> do
                        return $ bd{overlays=(sm,em,a):(overlays bd)}

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


------------------------------------------------------------------------

-- May need to resize buffer. How do we append to eof?
insertN' :: FBufferData -> [Char] -> Int -> IO FBufferData
insertN' fb [] _ = return fb
insertN' fb@(FBufferData _ _ _ old_end old_max hl ov) cs cs_len = do
        let need_len = old_end + cs_len
        (FBufferData ptr mks nms end mx _ _) <-
            if need_len >= old_max then resizeFB_ fb (need_len + 2048)
                                   else return fb
        let (MarkValue pnt _) = mks M.! pointMark
            len = max 0 (min (end - pnt) end) -- number of chars to shift
            dst = pnt + cs_len      -- point to start
            nend = dst + len        -- new length afterwards
        -- logPutStrLn $ "insertN' " ++ show cs ++ show pnt
        shiftChars ptr dst pnt len
        writeChars ptr cs pnt
        return (FBufferData ptr (shiftMarks pnt cs_len mks) nms nend mx hl ov)
{-# INLINE insertN' #-}

shiftMarks :: Point -> Int -> Marks -> Marks
shiftMarks from by = M.map $ \(MarkValue p leftBound) -> (MarkValue (shift p leftBound) leftBound)
    where shift p leftBound | p < from  = p
                            | p == from = if leftBound then p else p'
                            | otherwise {- p > from -} = p'
                     where p' = max from (p + by)

------------------------------------------------------------------------

-- | @deleteN' b n p@ deletes @n@ characters forwards from position @p@
deleteN' :: FBufferData -> Int -> Int -> IO FBufferData
deleteN' b 0 _ = return b
deleteN' (FBufferData ptr mks nms end mx hl ov) n pos = do
        let src = inBounds (pos + n) end     -- start shifting back from
            len = inBounds (end-pos-n) end   -- length of shift
            end'= pos + len                  -- new end
        shiftChars ptr pos src len
        return (FBufferData ptr (shiftMarks pos (negate n) mks) nms end' mx hl ov)
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
finaliseBI fb = withMVar fb $ \(FBufferData ptr _ _ _ _ _ _) -> free ptr

-- | Number of characters in the buffer
sizeBI      :: BufferImpl -> IO Int
sizeBI fb = withMVar fb $ \(FBufferData _ _ _ n _ _ _) -> return n

-- | Extract the current point
pointBI     :: BufferImpl -> IO Int
pointBI fb = withMVar fb $ \(FBufferData _ mks _ e mx _ _) -> do
    let p = markPosition $ mks M.! pointMark
    assert ((p >= 0 && (p <= e || e == 0)) && e <= mx) $ return p
{-# INLINE pointBI #-}


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI    :: BufferImpl -> Int -> Int -> IO [Char]
nelemsBI fb n i = withMVar fb $ \(FBufferData b _ _ e _ _ _) -> do
        let i' = inBounds i e
            n' = min (e-i') n
        readChars b n' i'

-- | Return @n@ elems starting at @i@ of the buffer as a list.
-- This routine also does syntax highlighting and applies overlays.
nelemsBIH    :: BufferImpl -> Int -> Int -> IO [(Char,Attr)]
nelemsBIH fb n i = withMVar fb fun
    where
      -- The first case is to handle when no 'Highlighter a' has
      -- been assigned to the buffer (via eg 'setSyntaxBI bi "haskell"')
      fun bd@(FBufferData b _ _ e _ Nothing _) = do
             let i' = inBounds i e
                 n' = min (e-i') n
             cas <- fmap (map (flip (,) attr)) (readChars b n' i')
             return $ overlay bd cas
      -- in this, second, case 'hl' will be bound to a 'Highlighter a'
      -- eg Yi.Syntax.Haskell.highlighter (see Yi.Syntax for defn of Highlighter) which
      -- uses '(Data.ByteString.Char8.ByteString, Int)' as its parameterized state
      fun bd@(FBufferData b _ _ e _ (Just (HLState hl)) _) = do
        bs <- B.copyCStringLen (b, e)
        let (finst,colors_) = hlColorize hl bs (hlStartState hl)
            colors = colors_ ++ hlColorizeEOF hl finst
        return $ overlay bd (take n (drop i (zip (B.unpack bs) (map styleToAttr colors))))

      overlay :: FBufferData -> [(Char,Attr)] -> [(Char,Attr)]
      overlay bd xs = map (\((c,a),j)->(c, ov bd (a,j))) (zip xs [i..])

      ov :: FBufferData -> (Attr, Int) -> Attr
      ov bd (sh_attr, ind) = foldr (\(sm, em, a) att ->
                                    if betweenMarks (marks bd) sm em ind
                                    then a `attrOver` att
                                    else att)
                             sh_attr (overlays bd)

      betweenMarks mks m1 m2 pos = pos>=(markPosition (mks M.! m1)) && pos<(markPosition (mks M.! m2))

      --attrOver att1 att2 = att1 .|. (att2 .&. 0xFFFF0000) -- Overwrite colors, keep attrs (bold, underline etc)
      attrOver att1 _att2 = att1 -- Until Vty exposes interface for attr merging....

------------------------------------------------------------------------
-- Point based editing

-- | Move point in buffer to the given index
moveToI     :: BufferImpl -> Int -> IO ()
moveToI fb i = modifyMVar_ fb $ \(FBufferData ptr mks nms end mx hl ov) -> do
                 logPutStrLn $ "moveToI: " ++ show i
                 return $ FBufferData ptr (M.insert pointMark (MarkValue (inBounds i end) pointLeftBound) mks) nms end mx hl ov
{-# INLINE moveToI #-}


-- | Write an element into the buffer at the current point
writeBI :: BufferImpl -> Char -> IO ()
writeBI fb c = withMVar fb $ \ (FBufferData ptr mks _ _ _ _ _) -> do
        let off = markPosition (mks M.! pointMark)
        writeChars ptr [c] off
{-# INLINE writeBI #-}

-- | Insert the list at current point, extending size of buffer
insertNI    :: BufferImpl -> [Char] -> IO ()
insertNI fb cs = modifyMVar_ fb $ \fb'-> insertN' fb' cs (length cs)

-- | @deleteNAtI b n p@ deletes @n@ characters forwards from position @p@
deleteNAtI :: BufferImpl -> Int -> Int -> IO ()
deleteNAtI fb n pos = modifyMVar_ fb $ \fb' -> deleteN' fb' n pos

------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLnI       :: BufferImpl -> IO Int
-- count number of \n from origin to point
curLnI fb = withMVar fb $ \(FBufferData ptr mks _ _ _ _ _) -> ccountLines ptr 0 $ markPosition $ mks M.! pointMark
{-# INLINE curLnI #-}

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLnI      :: BufferImpl -> Int -> IO Int
gotoLnI fb n = modifyMVar fb $ \(FBufferData ptr mks nms e mx hl ov) -> do
        np <- cfindStartOfLineN ptr 0 e (n-1)       -- index from 0
        let fb' = FBufferData ptr (M.insert pointMark (MarkValue np pointLeftBound) mks) nms e mx hl ov
        n' <- if np > e - 1 -- if next line is end of file, then find out what line this is
              then return . subtract 1 =<< ccountLines ptr 0 np
              else return n         -- else it is this line
        return (fb', max 1 n')
{-# INLINE gotoLnI #-}


    ---------------------------------------------------------------------




-- | Return index of next string in buffer that matches argument
searchBI      :: BufferImpl -> [Char] -> IO (Maybe Int)
searchBI fb s = withMVar fb $ \(FBufferData ptr mks _ _ _ _ _) -> 
        withCString s $ \str -> do
            p <- cstrstr (ptr `advancePtr` (markPosition $ mks M.! pointMark)) str
            return $ if p == nullPtr then Nothing
                                     else Just (p `minusPtr` ptr)

-- | Return indices of next string in buffer matched by regex
regexBI       :: BufferImpl -> Regex -> IO (Maybe (Int,Int))
regexBI fb re = withMVar fb $ \(FBufferData ptr mks _ _ _ _ _) -> do
        let p = markPosition $ mks M.! pointMark
        Right mmatch <- wrapMatch re (ptr `plusPtr` p)
        logPutStrLn $ show mmatch
        case mmatch of
            Nothing        -> return Nothing
            Just []        -> return Nothing
            Just ((i,j):_) -> return (Just (p+fromIntegral i,p+fromIntegral j))    -- offset from point


-- ------------------------------------------------------------------------
    ---------------------------------------------------------------------


{- 
   Okay if the mark is set then we return that, otherwise we
   return the point, which will mean that the calling function will
   see the selection area as null in length. 
-}
getSelectionMarkBI :: BufferImpl -> IO Mark
getSelectionMarkBI fb = withMVar fb $ \(FBufferData { marks = marksMap } ) -> return $ if M.member markMark marksMap then markMark else pointMark

-- | Returns ths position of the 'point' mark if the requested mark is unknown
getMarkPointBI :: BufferImpl -> Mark -> IO Point
getMarkPointBI fb m = do
                      mv <- getMark fb m
                      logPutStrLn $ "get mark " ++ show m ++ " at " ++ show mv
                      return $ markPosition mv

getMark :: BufferImpl -> Mark -> IO MarkValue
getMark fb m = withMVar fb $ \(FBufferData { marks = marksMap } ) -> do
                 return $ M.findWithDefault (marksMap M.! pointMark) m marksMap
                 -- We look up mark m in the marks, the default value to return
                 -- if mark m is not set, is the pointMark


-- | Set this buffer mark
setMarkPointBI :: BufferImpl -> Mark -> Point -> IO ()
setMarkPointBI fb m pos = modifyMVar_ fb $ \fb' -> do
                            logPutStrLn $ "set mark " ++ show m ++ " at " ++ show pos
                            let marks' = M.insert m (MarkValue pos (if m == markMark then markLeftBound else False)) (marks fb')
                            logPutStrLn $ "marks: " ++ show marks'
                            return $ fb' {marks = marks'}

{-
  We must allow the unsetting of this mark, this will have the property
  that the point will always be returned as the mark.
-}
unsetMarkBI      :: BufferImpl -> IO ()
unsetMarkBI fb = modifyMVar_ fb $ \fb'-> return $ fb' { marks = (M.delete markMark (marks fb')) }

-- Basically this function just takes the name of a 'Highlighter a' (from Yi.Syntax.Table)
-- and sets that to be the current highlighter for this buffer.
setSyntaxBI      :: BufferImpl -> String -> IO ()
setSyntaxBI fb sy = modifyMVar_ fb $ \fb' -> do (ExtHL e) <- evaluate (highlighters M.! sy)
                                                return fb' { hlcache = HLState `fmap` e }

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

-- | Returns the requested mark, creating a new mark with that name (at point) if needed
getMarkBI :: BufferImpl -> Maybe String -> IO Mark
getMarkBI b name = pointBI b >>= getMarkDefaultPosBI b name

-- | Returns the requested mark, creating a new mark with that name (at the supplied position) if needed
getMarkDefaultPosBI :: BufferImpl -> Maybe String -> Int -> IO Mark
getMarkDefaultPosBI b name defaultPos = modifyMVar b $ \ fb@(FBufferData ptr mks nms end mx hl ov) -> do
  logPutStrLn $ "getMarkBI: " ++ show nms ++ ", " ++ show mks
  let m :: Maybe Mark = flip M.lookup nms =<< name
  case m of
    Just m' -> do
           logPutStrLn $ "found mark: " ++ show name ++ " = " ++ show m'
           return (fb, m')
    Nothing -> do
           let newMark = Mark (1 + max 1 (markId $ fst (M.findMax mks)))
           let nms' = case name of
                        Nothing -> nms
                        Just nm -> M.insert nm newMark nms
           let mks' = M.insert newMark (MarkValue defaultPos False) mks
           logPutStrLn $ "new mark: " ++ show name ++ " = " ++ show newMark
           return (FBufferData ptr mks' nms' end mx hl ov, newMark)

