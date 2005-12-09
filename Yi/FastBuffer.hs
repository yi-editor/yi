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

module Yi.FastBuffer (FBuffer(..), BufferMode(..), FBuffer_(..)) where

import Yi.Buffer
import Yi.Regex
import Yi.Undo

import Data.Unique              ( Unique, newUnique )
import qualified Data.Map as M

import Control.Monad            ( when )
import Control.Exception        ( assert )
import Control.Concurrent.MVar

import System.IO                ( openFile, hGetBuf, hPutBuf,
                                  hFileSize, hClose, hFlush, IOMode(..) )

import Foreign.C.String
import Foreign.C.Types          ( CChar )
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc    ( free )
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr              ( Ptr, nullPtr, minusPtr )
import Foreign.Storable         ( poke )

-- ---------------------------------------------------------------------
--
-- | Fast buffer based on the implementation of 'Handle' in
-- 'GHC.IOBase' and 'GHC.Handle'. The buffer itself is stored as a
-- mutable byte array. Also helped by ghc/utils/StringBuffer.lhs, and
-- helpful criticism from Manuel Chakravarty (on why the FFI is a
-- *good thing*)
--
-- In the concurrent world, buffers are locked during use.
--
data BufferMode = ReadOnly | ReadWrite

data FBuffer =
        FBuffer { name   :: !String           -- immutable buffer name
                , bkey   :: !Unique           -- immutable unique key
                , file   :: !(MVar (Maybe FilePath)) -- maybe a filename associated with this buffer
                , undos  :: !(MVar URList)      -- undo/redo list
                , rawbuf :: !(MVar FBuffer_)
                , bmode  :: !(MVar BufferMode)  -- a read-only bit
                }

data FBuffer_ =
        FBuffer_ { _rawmem  :: !(Ptr CChar)     -- raw memory           (ToDo unicode)
                 , marks    :: !(M.Map Int Int) -- 0: point, 1: mark
                   -- TODO: This isn't the most efficient structure
                   -- to store marks. When IntMap gets stabilized we'd
                   -- better switch to it.
                 , _contsize :: !Int             -- length of contents
                 , _rawsize  :: !Int             -- raw size of buffer
                 }
instance Eq FBuffer where
   FBuffer { bkey = u } == FBuffer { bkey = v } = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer { name = f }) = showString $ "\"" ++ f ++ "\""

-- ---------------------------------------------------------------------
--
-- | Creation. Get a new 'FBuffer' filled from FilePath.
--
hNewFBuffer :: FilePath -> IO FBuffer
hNewFBuffer f = do
    h    <- openFile f ReadMode
    size <- hFileSize h
    let size_i = fromIntegral size
        r_size = size_i + 2048
    ptr <- mallocArray0 r_size
    r <- if size_i == 0 then return 0 else hGetBuf h ptr size_i
    hClose h
    if (r /= size_i)
        then ioError (userError $ "Short read of file: " ++ f)
        else do poke (ptr `advancePtr` size_i) (castCharToCChar '\0')
                mv  <- newMVar  (FBuffer_ ptr (M.fromList [(0,0), (1,0)]) size_i r_size)
                mv' <- newMVar  emptyUR
                fn  <- newMVar  (Just f)        -- filename is buffer name
                rw  <- newMVar  ReadWrite
                u   <- newUnique
                return $ FBuffer { name = f
                                 , bkey   = u
                                 , file   = fn
                                 , undos  = mv'
                                 , rawbuf = mv
                                 , bmode  = rw }

--
-- | Write contents of buffer into specified file
--
hPutFBuffer_ :: FBuffer_ -> FilePath -> IO ()
hPutFBuffer_ (FBuffer_ bytearr _ end _) f = do
    h <- openFile f WriteMode
    hPutBuf h bytearr end
    hFlush h
    hClose h

--
-- | Resize an FBuffer_
--
resizeFB_ :: FBuffer_ -> Int -> IO FBuffer_
resizeFB_ (FBuffer_ ptr p e _) sz = do
    ptr' <- reallocArray0 ptr sz
    return (FBuffer_ ptr' p e sz)

--
-- | New FBuffer filled from string.
--
stringToFBuffer :: String -> String -> IO FBuffer
stringToFBuffer nm s = do
    let size_i = length s
        r_size = size_i + 2048
    ptr <- mallocArray0 r_size
    pokeArray ptr (map castCharToCChar s) -- Unicode
    poke (ptr `advancePtr` size_i) (castCharToCChar '\0')
    mv  <- newMVar (FBuffer_ ptr (M.fromList [(0,0), (1,0)]) size_i r_size)
    mv' <- newMVar emptyUR
    mvf <- newMVar Nothing      -- has name, not connected to a file
    rw  <- newMVar ReadWrite
    u   <- newUnique
    return $ FBuffer { name   = nm
                     , bkey   = u
                     , file   = mvf
                     , undos  = mv'
                     , rawbuf = mv
                     , bmode  = rw }

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

foreign import ccall unsafe "YiUtils.h expandedLengthOfStr"
   cexpandedTabLength :: Ptr CChar -> Int -> Int -> Int -> IO Int

foreign import ccall unsafe "YiUtils.h strlenWithExpandedLengthN"
   cfindlength :: Ptr CChar -> Int -> Int -> Int -> Int -> IO Int

------------------------------------------------------------------------

-- May need to resize buffer. How do we append to eof?
insertN' :: FBuffer -> [Char] -> Int -> IO ()
insertN'  _ [] _ = return ()
insertN' (FBuffer { rawbuf = mv }) cs cs_len =
    modifyMVar_ mv $ \fb@(FBuffer_ _ _ old_end old_max) -> do
        let need_len = old_end + cs_len
        (FBuffer_ ptr pnts end mx) <-
            if need_len >= old_max then resizeFB_ fb (need_len + 2048)
                                   else return fb
        let pnt = pnts M.! 0
            len = max 0 (min (end-pnt) end) -- number of chars to shift
            dst = pnt + cs_len      -- point to start
            nend = dst + len        -- new length afterwards
        shiftChars ptr dst pnt len
        writeChars ptr cs pnt
        let pnts' = M.map (\p -> if p > pnt then p + cs_len else p) pnts
        return (FBuffer_ ptr pnts' nend mx)
{-# INLINE insertN' #-}

-- Same as above, except we use copyBytes, instead of writeChars
-- Refactor, please
insertFromCStrN' :: FBuffer -> (Ptr CChar) -> Int -> IO ()
insertFromCStrN'  _ _ 0 = return ()
insertFromCStrN' (FBuffer { rawbuf = mv }) cptr cs_len =
    modifyMVar_ mv $ \fb@(FBuffer_ _ _ old_end old_max) -> do
        let need_len = old_end + cs_len
        (FBuffer_ ptr pnts end mx) <-
            if need_len >= old_max then resizeFB_ fb (need_len + 2048)
                                   else return fb
        let pnt = pnts M.! 0
            len = max 0 (min (end-pnt) end) -- number of chars to shift
            dst = pnt + cs_len      -- point to start
            nend = dst + len        -- new length afterwards
        shiftChars ptr dst pnt len
        copyBytes (ptr `advancePtr` pnt) cptr cs_len
        let pnts' = M.map (\p -> if p > pnt then p + cs_len else p) pnts
        return (FBuffer_ ptr pnts' nend mx)

------------------------------------------------------------------------

deleteN' :: FBuffer -> Int -> Int -> IO ()
deleteN' _ 0 _ = return ()
deleteN' (FBuffer { rawbuf = mv }) n pos =
    modifyMVar_ mv $ \(FBuffer_ ptr pnts end mx) -> do
        let src = inBounds (pos + n) end     -- start shifting back from
            len = inBounds (end-pos-n) end   -- length of shift
            end'= pos + len                  -- new end
        shiftChars ptr pos src len
        let pnts' = M.map shift pnts
            shift p | p < pos = p
                    | p < end' = pos
                    | otherwise = p - len
        return (FBuffer_ ptr pnts' end' mx)
{-# INLINE deleteN' #-}

------------------------------------------------------------------------
--
-- | 'FBuffer' is a member of the 'Buffer' class, providing fast
-- indexing operations. It is implemented in terms of a mutable byte
-- array.
--

instance Buffer FBuffer where

    -- newB :: String -> [Char] -> IO a
    newB = stringToFBuffer

    -- finaliseB :: a -> IO ()
    finaliseB (FBuffer { rawbuf = mv }) = do
        (FBuffer_ ptr _ _ _) <- readMVar mv
        free ptr

    -- hNewB :: FilePath -> IO a
    hNewB = hNewFBuffer

    -- hPutB :: a -> FilePath -> IO ()
    hPutB (FBuffer { rawbuf = mv }) f = readMVar mv >>= flip hPutFBuffer_ f

    -- nameB :: a -> String
    nameB (FBuffer { name = n }) = n

    -- filenameB :: a -> IO (Maybe FilePath)
    getfileB (FBuffer { file = mvf }) = readMVar mvf

    -- setfileB :: a -> FilePath -> IO ()
    setfileB (FBuffer { file = mvf }) f =
        modifyMVar_ mvf $ const $ return (Just f)

    -- keyB :: a -> Unique
    keyB (FBuffer { bkey = u }) = u

    -- sizeB      :: a -> IO Int
    sizeB (FBuffer { rawbuf = mv }) = do
        (FBuffer_ _ _ n _) <- readMVar mv
        return n

    -- pointB     :: a -> IO Int
    pointB (FBuffer { rawbuf = mv }) = do
        (FBuffer_ _ pnts e mx) <- readMVar mv
        let p = (pnts M.! 0)
        assert ((p >= 0 && (p < e || e == 0)) && e <= mx) $ return p
    {-# INLINE pointB #-}

    -- isUnchangedB  :: a -> IO Bool
    isUnchangedB (FBuffer { undos = mv }) = do
        ur <- readMVar mv
        return $ isEmptyUList ur

    ------------------------------------------------------------------------

    -- elemsB     :: a -> IO [Char]
    elemsB (FBuffer { rawbuf = mv }) =
        withMVar mv $ \(FBuffer_ b _ n _) -> readChars b n 0

    -- nelemsB    :: a -> Int -> Int -> IO [Char]
    nelemsB (FBuffer { rawbuf = mv }) n i =
        withMVar mv $ \(FBuffer_ b _ e _) -> do
            let i' = inBounds i e
                n' = min (e-i') n
            readChars b n' i'

    -- ptrToLnsB  :: a -> Int -> Int -> Int -> IO [CStringLen]
    ptrToLnsB (FBuffer { rawbuf = mv }) i' len width =
        withMVar mv $ \(FBuffer_ ptr _ e _) -> do
            let i = inBounds i' e
                loop 0 _ = return []
                loop n j | j >= e    = return []
                         | otherwise = do
                            let this = ptr `advancePtr` j -- new line
                            off  <- cfindStartOfLineN this 0 (max 0 (e - j)) 1
                            size <- cfindlength this 0 off 8{-tab-} width
                            ptrs <- loop (n-1) (j+off)
                            return ((this,size) : ptrs)
            loop len i

    ------------------------------------------------------------------------

    -- moveTo     :: a -> Int -> IO ()
    moveTo (FBuffer { rawbuf = mv }) i =
        modifyMVar_ mv $ \(FBuffer_ ptr pnts end mx) ->
            return $ FBuffer_ ptr (M.insert 0 (inBounds i end) pnts) end mx
    {-# INLINE moveTo #-}

    -- readB      :: a -> IO Char
    readB (FBuffer { rawbuf = mv }) =
        withMVar mv $ \(FBuffer_ ptr pnts _ _) ->
            readChars ptr 1 (pnts M.! 0) >>= \[c] -> return c
    {-# INLINE readB #-}

    -- readAtB :: a -> Int -> IO Char
    -- should perform a bounds check? (readAtB n on an empty buffer is unsafe)
    readAtB (FBuffer { rawbuf = mv }) off =
        withMVar mv $ \(FBuffer_ ptr _ e _) ->
            readChars ptr 1 (inBounds off e) >>= \[c] -> return c

    ------------------------------------------------------------------------
    -- TODO undo

    -- writeB :: a -> Char -> IO ()
    writeB (FBuffer { undos = uv, rawbuf = mv }) c =
        withMVar mv $ \(FBuffer_ ptr pnts _ _) -> do
            let off = (pnts M.! 0)
            modifyMVar_ uv $ \u -> do
                ins <- mkInsert ptr off 1
                let u'  = addUR u  ins
                    u'' = addUR u' (mkDelete off 1)
                return u''
            writeChars ptr [c] off
    {-# INLINE writeB #-}

    ------------------------------------------------------------------------

    -- insertB :: a -> Char -> IO ()
    insertB a c = insertN a [c]

    -- insertN :: a -> [Char] -> IO ()
    insertN  _ [] = return ()
    insertN fb@(FBuffer { undos = uv, rawbuf = mv}) cs = do
        (FBuffer_ _ pnts _ _) <- readMVar mv
        let cs_len = length cs
            pnt = pnts M.! 0
        modifyMVar_ uv $ \ur -> return $ addUR ur (mkDelete pnt cs_len)
        insertN' fb cs cs_len

    ------------------------------------------------------------------------
    -- deleteB    :: a -> IO ()
    deleteB a = deleteN a 1

    -- deleteN    :: a -> Int -> IO ()
    deleteN _ 0 = return ()
    deleteN b n = do
        point <- pointB b
        deleteNAt b n point

    -- deleteNAt :: a -> Int -> Int -> IO ()
    deleteNAt _ 0 _ = return ()
    deleteNAt fb@(FBuffer { undos = uv, rawbuf = mv }) n pos = do
        -- quick! before we delete the chars, copy them to the redo buffer
        (FBuffer_ ptr _ end _) <- readMVar mv
        modifyMVar_ uv $ \ur -> do
            ins <- mkInsert ptr pos (max 0 (min n (end-pos))) -- something wrong.
            return $ addUR ur ins
        deleteN' fb n pos  -- now, really delete

    ------------------------------------------------------------------------

    -- undo        :: a -> IO ()
    undo fb@(FBuffer { undos = mv }) = modifyMVar_ mv (undoUR fb)

    -- redo        :: a -> IO ()
    redo fb@(FBuffer { undos = mv }) = modifyMVar_ mv (redoUR fb)

    getActionB = getActionFB

    ------------------------------------------------------------------------

    -- atSol       :: a -> IO Bool -- or at start of file
    atSol a = do p <- pointB a
                 if p == 0 then return True
                           else do c <- readAtB a (p-1)
                                   return (c == '\n')
    {-# INLINE atSol #-}

    -- atEol       :: a -> IO Bool -- or at end of file
    atEol a = do p <- pointB a
                 e <- sizeB a
                 if p == max 0 (e-1)
                        then return True
                        else do c <- readB a
                                return (c == '\n')
    {-# INLINE atEol #-}

    -- atEof       :: a -> IO Bool
    atEof a = do p <- pointB a
                 e <- sizeB a
                 return (p == max 0 (e-1))
    {-# INLINE atEof #-}

    -- atSof       :: a -> IO Bool
    atSof a = do p <- pointB a
                 return (p == 0)
    {-# INLINE atSof #-}

    -- atLastLine  :: a -> IO Bool
    atLastLine b = do
            p <- pointB b
            moveToEol b
            e <- atEof b
            moveTo b p
            return e
    {-# INLINE atLastLine #-}

    ------------------------------------------------------------------------

    -- moveToSol   :: a -> IO ()
    -- optimised. crucial for long lines
    -- moveToSol a = sizeB a >>= moveXorSol a
    moveToSol (FBuffer { rawbuf = mv }) =
        modifyMVar_ mv $ \(FBuffer_ ptr pnts end mx) -> do
            let p = pnts M.! 0
            off <- cfindStartOfLineN ptr p 0 (-1)
            return $ FBuffer_ ptr (M.insert 0 (inBounds (p + off) end) pnts) end mx
    {-# INLINE moveToSol #-}

    -- moveToEol   :: a -> IO ()
    -- optimised. crucial for long lines
    --  was:     moveToEol a = sizeB a >>= moveXorEol a
    moveToEol (FBuffer { rawbuf = mv }) =
        modifyMVar_ mv $ \(FBuffer_ ptr pnts end mx) -> do
            let p = pnts M.! 0
            off <- cfindStartOfLineN ptr p end 1 -- next line
            return $ FBuffer_ ptr (M.insert 0 (inBounds (p+off-1) end) pnts) end mx
    {-# INLINE moveToEol #-}

    -- offsetFromSol :: a -> IO Int
    offsetFromSol a = do
        i <- pointB a
        moveToSol a
        j <- pointB a
        moveTo a i
        return (i - j)
    {-# INLINE offsetFromSol #-}

    -- indexOfSol   :: a -> IO Int
    indexOfSol a = do
        i <- pointB a
        j <- offsetFromSol a
        return (i - j)
    {-# INLINE indexOfSol #-}

    -- indexOfEol   :: a -> IO Int
    indexOfEol a = do
        i <- pointB a
        moveToEol a
        j <- pointB a
        moveTo a i
        return j
    {-# INLINE indexOfEol #-}

    -- indexOfNLFrom :: a -> Int -> IO Int
    indexOfNLFrom b i = do
        p <- pointB b
        moveTo b i
        lineDown b
        q <- pointB b
        moveTo b p
        return q
    {-# INLINE indexOfNLFrom #-}

    -- moveAXuntil :: a -> (a -> IO ()) -> Int -> (a -> IO Bool) -> IO ()
    -- will be slow on long lines...
    moveAXuntil b f x p
        | x <= 0    = return ()
        | otherwise = do
            let loop 0 = return ()
                loop i = do r <- p b
                            when (not r) $ f b >> loop (i-1)
            loop x
    {-# INLINE moveAXuntil #-}

    ------------------------------------------------------------------------

    -- deleteToEol :: a -> IO ()
    deleteToEol b = do
        p <- pointB b
        moveToEol b
        q <- pointB b
        c <- readB b
        let r = fromEnum $ c /= '\n' -- correct for eof
        moveTo b p
        deleteN b (max 0 (q-p+r))
    {-# INLINE deleteToEol #-}

    -- ---------------------------------------------------------------------
    -- Line based movement and friends

    -- lineUp :: a -> IO ()
    lineUp b = do
        x <- offsetFromSol b
        moveToSol b
        leftB b
        moveToSol b
        moveXorEol b x
    {-# INLINE lineUp #-}

    -- lineDown :: a -> IO ()
    lineDown b = do
        x <- offsetFromSol b
        moveToEol b
        rightB b
        moveXorEol b x
    {-# INLINE lineDown #-}

    ------------------------------------------------------------------------

    -- count number of \n from origin to point
    -- curLn :: a -> IO Int
    curLn (FBuffer { rawbuf = mv }) = withMVar mv $ \(FBuffer_ ptr pnts _ _) ->
        ccountLines ptr 0 $ pnts M.! 0
    {-# INLINE curLn #-}

    -- gotoLn :: a -> Int -> IO Int
    gotoLn (FBuffer { rawbuf = mv }) n =
        modifyMVar mv $ \(FBuffer_ ptr pnts e mx) -> do
            np <- cfindStartOfLineN ptr 0 e (n-1)       -- index from 0
            let fb = FBuffer_ ptr (M.insert 0 np pnts) e mx
            n' <- if np > e - 1 -- if next line is end of file, then find out what line this is
                  then return . subtract 1 =<< ccountLines ptr 0 np
                  else return n         -- else it is this line
            return (fb, max 1 n')
    {-# INLINE gotoLn #-}

    -- gotoLnFrom :: a -> Int -> IO Int
    gotoLnFrom (FBuffer { rawbuf = mv }) n =
        modifyMVar mv $ \(FBuffer_ ptr pnts e mx) -> do
            let p = pnts M.! 0
            off <- cfindStartOfLineN ptr p (if n < 0 then 0 else (e-1)) n
            let fb = FBuffer_ ptr (M.insert 0 (p + off) pnts) e mx
            ln <- return . subtract 1 =<< ccountLines ptr 0 (p+off) -- end of file
            return (fb, max 1 ln)
    {-# INLINE gotoLnFrom #-}

    -- ---------------------------------------------------------------------

    -- searchB      :: a -> [Char] -> IO (Maybe Int)
    searchB (FBuffer { rawbuf = mv }) s =
        withMVar mv $ \(FBuffer_ ptr pnts _ _) ->
            withCString s $ \str -> do
                p <- cstrstr (ptr `advancePtr` (pnts M.! 0)) str
                return $ if p == nullPtr then Nothing
                                         else Just (p `minusPtr` ptr)

    -- regexB       :: a -> Regex -> IO (Maybe (Int,Int))
    regexB (FBuffer { rawbuf = mv }) re =
        withMVar mv $ \(FBuffer_ ptr pnts _ _) -> do
            let p = (pnts M.! 0)
            mmatch <- regexec re ptr p
            case mmatch of
                Nothing        -> return Nothing
                Just ((i,j),_) -> return (Just (p+i,p+j))    -- offset from point


    -- ------------------------------------------------------------------------

    getMarkB (FBuffer { rawbuf = mv }) =
        withMVar mv $ \(FBuffer_ {marks=p}) -> return (p M.! 1)

    setMarkB (FBuffer { rawbuf = mv }) pos =
        modifyMVar_ mv $ \fb -> return $ fb {marks = (M.insert 1 pos (marks fb))}

    -- ------------------------------------------------------------------------

    expandedTabLengthB b@(FBuffer { rawbuf = mv }) width = do
        i <- indexOfSol b
        withMVar mv $ \(FBuffer_ ptr pnts _ _) -> do
            let j = pnts M.! 0
            cexpandedTabLength ptr i j width

------------------------------------------------------------------------

-- | calculate whether a move is in bounds.
inBounds :: Int -> Int -> Int
inBounds i end | i <= 0    = 0
               | i >= end  = max 0 (end - 1)
               | otherwise = i
{-# INLINE inBounds #-}

-- ---------------------------------------------------------------------
-- Support for generic undo (see LinearUndo module)

-- Given a URAction, return the buffer action it represents, and the
-- URAction that reverses it.
--
getActionFB :: URAction -> (FBuffer -> IO URAction)
getActionFB (Delete p n) b@(FBuffer { rawbuf = mv }) = do
    (FBuffer_ ptr _ _ _) <- readMVar mv
    moveTo b p
    p' <- pointB b
    ins <- mkInsert ptr p' n
    deleteN' b n p'       -- need to be actions that don't in turn invoke the url
    return ins

getActionFB (Insert p n fptr) b = do
    moveTo b p
    withForeignPtr fptr $ \ptr -> insertFromCStrN' b ptr n
    p' <- pointB b
    return $ mkDelete p' n

-- Create an insert action
mkInsert :: (Ptr CChar) -> Point -> Size -> IO URAction
mkInsert ptr off n = do
    fptr <- mallocForeignPtrBytes n
    withForeignPtr fptr $ \fp -> copyBytes fp (ptr `advancePtr` off) n
    return (Insert off n fptr)

-- Create a delete action
mkDelete :: Point -> Size -> URAction
mkDelete = Delete
