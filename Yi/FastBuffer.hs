{-# OPTIONS -fffi -#include YiUtils.h #-}
-- 
-- Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

module Yi.FastBuffer (FBuffer(..)) where

import Yi.Buffer
import Yi.Regex

import Data.Unique              ( Unique, newUnique )

import Control.Exception        ( assert )
import Control.Concurrent.MVar

import IO                       ( hFileSize, hClose, hFlush, IOMode(..) )
import System.IO                ( openBinaryFile, hGetBuf, hPutBuf )

import Foreign.C.String
import Foreign.C.Types          ( CChar )
import Foreign.Marshal.Alloc    ( free )
import Foreign.Marshal.Array
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

data FBuffer = 
        FBuffer !FilePath        -- immutable name
                !Unique          -- immutable unique key
                !(MVar URList)   -- undo/redo list
                !(MVar FBuffer_)

data FBuffer_ = 
        FBuffer_ !(Ptr CChar)   -- raw memory           (ToDo unicode)
                 !Int           -- current position     (ToDo list of pnts)
                 !Int           -- length of contents
                 !Int           -- raw size of buffer

instance Eq FBuffer where
   (FBuffer _ u _ _) == (FBuffer _ v _ _) = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer f _ _ _) = showString $ "\"" ++ f ++ "\""

-- ---------------------------------------------------------------------
--
-- | Creation. Get a new 'FBuffer' filled from FilePath.
--
hNewFBuffer :: FilePath -> IO FBuffer
hNewFBuffer f = do
    h    <- openBinaryFile f ReadMode
    size <- hFileSize h
    let size_i = fromIntegral size
        r_size = size_i + 2048
    ptr <- mallocArray0 r_size
    r <- if size_i == 0 then return 0 else hGetBuf h ptr size_i
    if (r /= size_i)
        then ioError (userError $ "Short read of file: " ++ f)
        else do poke (ptr `advancePtr` size_i) (castCharToCChar '\0')
                mv  <- newMVar  (FBuffer_ ptr 0 size_i r_size)
                mv' <- newMVar  emptyUR
                u   <- newUnique
                return (FBuffer f u mv' mv)

--
-- | Write contents of buffer into specified file
--
hPutFBuffer_ :: FBuffer_ -> FilePath -> IO ()
hPutFBuffer_ (FBuffer_ bytearr _ end _) f = do
    h <- openBinaryFile f WriteMode
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
    mv  <- newMVar (FBuffer_ ptr 0 size_i r_size)
    mv' <- newMVar emptyUR
    u   <- newUnique
    return (FBuffer nm u mv' mv)

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

foreign import ccall unsafe "countlns"
   ccountlns :: Ptr CChar -> Int -> Int -> IO Int

foreign import ccall unsafe "gotoln"
   cgotoln :: Ptr CChar -> Int -> Int -> Int -> IO Int

------------------------------------------------------------------------

-- May need to resize buffer. How do we append to eof?
insertN' :: FBuffer -> [Char] -> Int -> IO ()
insertN'  _ [] _ = return ()
insertN' (FBuffer _ _ _ mv) cs cs_len = 
    modifyMVar_ mv $ \fb@(FBuffer_ _ _ old_end old_max) -> do
        let need_len = old_end + cs_len
        (FBuffer_ ptr pnt end mx) <- 
            if need_len >= old_max then resizeFB_ fb (need_len + 2048) 
                                   else return fb
        let len = max 0 (min (end-pnt) end) -- number of chars to shift
            dst = pnt + cs_len      -- point to start
            nend = dst + len        -- new length afterwards
        shiftChars ptr dst pnt len
        writeChars ptr cs pnt
        return (FBuffer_ ptr pnt nend mx)
{-# INLINE insertN' #-}

deleteN' :: FBuffer -> Int -> IO ()
deleteN' _ 0 = return ()
deleteN' (FBuffer _ _ _ mv) n = 
    modifyMVar_ mv $ \(FBuffer_ ptr pnt end mx) -> do
        let src = inBounds (pnt + n) end     -- start shifting back from
            len = inBounds (end-pnt-n) end   -- length of shift
            end'= pnt + len                  -- new end
        shiftChars ptr pnt src len
        let pnt' | pnt == 0    = pnt
                 | pnt == end' = max 0 (pnt - 1)    -- shift back if at eof
                 | otherwise   = pnt
        return (FBuffer_ ptr pnt' end' mx)
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
    finaliseB (FBuffer _ _ _ mv) = do
        (FBuffer_ ptr _ _ _) <- readMVar mv
        free ptr

    -- hNewB :: FilePath -> IO a
    hNewB = hNewFBuffer

    -- hPutB :: a -> FilePath -> IO ()
    hPutB (FBuffer _ _ _ mv) f = readMVar mv >>= flip hPutFBuffer_ f

    -- nameB :: a -> String
    nameB (FBuffer f _ _ _) = f

    -- keyB :: a -> Unique
    keyB (FBuffer _ u _ _) = u

    -- sizeB      :: a -> IO Int
    sizeB (FBuffer _ _ _ mv) = do
        (FBuffer_ _ _ n _) <- readMVar mv
        return n

    -- pointB     :: a -> IO Int
    pointB (FBuffer _ _ _ mv) = do
        (FBuffer_ _ p e mx) <- readMVar mv
        assert ((p >= 0 && (p < e || e == 0)) && e <= mx) $ return p
    {-# INLINE pointB #-}

    ------------------------------------------------------------------------

    -- elemsB     :: a -> IO [Char]
    elemsB (FBuffer _ _ _ mv) =
        withMVar mv $ \(FBuffer_ b _ n _) -> readChars b n 0

    -- nelemsB    :: a -> Int -> Int -> IO [Char]
    nelemsB (FBuffer _ _ _ mv) n i =
        withMVar mv $ \(FBuffer_ b _ e _) -> do
            let i' = inBounds i e
                n' = min (e-i') n
            readChars b n' i'

    -- ptrToLnsB  :: a -> Int -> Int -> IO [CStringLen]
    ptrToLnsB (FBuffer _ _ _ mv) i' len = 
        withMVar mv $ \(FBuffer_ ptr _ e _) -> do
            let i = inBounds i' e
                loop 0 _ = return []
                loop n j | j >= e    = return []
                         | otherwise = do 
                            let ptr' = ptr `advancePtr` j
                            sz   <- cgotoln ptr' 0 (max 0 (e - j)) 2{-from 1-}
                            ptrs <- loop (n-1) (j+sz)
                            return ((ptr',sz) : ptrs)

            loop len i

    ------------------------------------------------------------------------

    -- moveTo     :: a -> Int -> IO ()
    moveTo (FBuffer _ _ _ mv) i = 
        modifyMVar_ mv $ \(FBuffer_ ptr _ end mx) ->
            return $ FBuffer_ ptr (inBounds i end) end mx
    {-# INLINE moveTo #-}

    -- readB      :: a -> IO Char
    readB (FBuffer _ _ _ mv) = 
        withMVar mv $ \(FBuffer_ ptr off _ _) ->
            readChars ptr 1 off >>= \[c] -> return c
    {-# INLINE readB #-}

    -- readAtB :: a -> Int -> IO Char
    readAtB (FBuffer _ _ _ mv) off = 
        withMVar mv $ \(FBuffer_ ptr _ e _) ->
            readChars ptr 1 (inBounds off e) >>= \[c] -> return c

    ------------------------------------------------------------------------
    -- TODO undo

    -- writeB :: a -> Char -> IO ()
    writeB (FBuffer _ _ uv mv) c = 
        withMVar mv $ \(FBuffer_ ptr off _ _) -> do
            modifyMVar_ uv $ \u -> do
                [x] <- readChars ptr 1 off 
                let u'  = addUR u  (Insert off x)   -- maybe a single action?
                    u'' = addUR u' (Delete off)
                return u''
            writeChars ptr [c] off
    {-# INLINE writeB #-}

    ------------------------------------------------------------------------

    -- insertB :: a -> Char -> IO ()
    insertB a c = insertN a [c]

    -- insertN :: a -> [Char] -> IO ()
    insertN  _ [] = return ()
    insertN fb@(FBuffer _ _ uv mv) cs = do
        (FBuffer_ _ pnt _ _) <- readMVar mv 
        let cs_len = length cs
        modifyMVar_ uv $ \ur ->     -- first update undo list
            let loop 0 u = u
                loop n u = let u' = addUR u (Delete pnt) in loop (n-1) u'
            in return $ loop cs_len ur
        insertN' fb cs cs_len       -- do the real insert

    ------------------------------------------------------------------------
    -- deleteB    :: a -> IO ()
    deleteB a = deleteN a 1

    -- deleteN    :: a -> Int -> IO ()
    deleteN _ 0 = return ()
    deleteN fb@(FBuffer _ _ uv mv) n = do
        -- quick! before we delete the chars, copy them to the redo buffer
        (FBuffer_ ptr pnt end _) <- readMVar mv 
        modifyMVar_ uv $ \ur ->
            let loop _ 0 u = return u
                loop p i u = do [x] <- readChars ptr 1 p    -- n.b
                                let u' = addUR u (Insert pnt x)
                                loop (p+1) (i-1) u'
            in do loop pnt (min n (end-pnt)) ur
        deleteN' fb n   -- now, really delete

    ------------------------------------------------------------------------
  
    -- undo        :: a -> IO ()
    undo fb@(FBuffer _ _ mv _) = modifyMVar_ mv (undoUR fb)

    -- redo        :: a -> IO ()
    redo fb@(FBuffer _ _ mv _) = modifyMVar_ mv (redoUR fb)

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

    ------------------------------------------------------------------------ 

    -- moveToSol   :: a -> IO ()
    moveToSol a = sizeB a >>= moveXorSol a
    {-# INLINE moveToSol #-}

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
        moveToSol a
        j <- pointB a
        moveTo a i
        return j
    {-# INLINE indexOfSol #-}

    -- indexOfEol   :: a -> IO Int
    indexOfEol a = do
        i <- pointB a
        moveToEol a
        j <- pointB a
        moveTo a i
        return j
    {-# INLINE indexOfEol #-}

    -- moveToEol   :: a -> IO ()
    moveToEol a = sizeB a >>= moveXorEol a
    {-# INLINE moveToEol #-}

    -- moveXorSol  :: a -> Int -> IO ()
    moveXorSol a x
        | x <= 0    = return ()
        | otherwise = do
            let loop 0 = return ()
                loop i = do sol <- atSol a
                            if sol then return () 
                                   else leftB a >> loop (i-1)
            loop x
    {-# INLINE moveXorSol #-}

    -- moveXorEol  :: a -> Int -> IO ()
    moveXorEol a x
        | x <= 0    = return ()
        | otherwise = do
            let loop 0 = return ()
                loop i = do eol <- atEol a
                            if eol then return () 
                                   else rightB a >> loop (i-1)
            loop x
    {-# INLINE moveXorEol #-}

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
    curLn (FBuffer _ _ _ mv) = withMVar mv $ \(FBuffer_ ptr i _ _) -> 
        ccountlns ptr 0 i
    {-# INLINE curLn #-}

    -- gotoLn :: a -> Int -> IO Int
    -- some supicious stuff in here..
    gotoLn (FBuffer _ _ _ mv) n = 
        modifyMVar mv $ \(FBuffer_ ptr _ e mx) -> do
            np <- cgotoln ptr 0 e n
            let fb = FBuffer_ ptr np e mx
            n' <- if np > e - 1 then return . subtract 1 =<< ccountlns ptr 0 np
                                else return n
            return (fb, max 1 n')  -- and deal with for n < 1
    {-# INLINE gotoLn #-}

    -- gotoLnFrom :: a -> Int -> IO Int
    gotoLnFrom (FBuffer _ _ _ mv) n = 
        modifyMVar mv $ \(FBuffer_ ptr p e mx) -> do
            off <- cgotoln ptr p (if n < 0 then 0 else (e-1)) n
            let fb = FBuffer_ ptr (p + off) e mx
            ln <- return . subtract 1 =<< ccountlns ptr 0 (p+off) -- hmm :(
            return (fb, max 1 ln)
    {-# INLINE gotoLnFrom #-}

    -- ---------------------------------------------------------------------

    -- searchB      :: a -> [Char] -> IO (Maybe Int)
    searchB (FBuffer _ _ _ mv) s =
        withMVar mv $ \(FBuffer_ ptr off _ _) ->
            withCString s $ \str -> do
                p <- cstrstr (ptr `advancePtr` off) str
                return $ if p == nullPtr then Nothing 
                                         else Just (p `minusPtr` ptr)

    -- regexB       :: a -> Regex -> IO (Maybe (Int,Int))
    regexB (FBuffer _ _ _ mv) re = 
        withMVar mv $ \(FBuffer_ ptr p _ _) -> do
            mmatch <- regexec re ptr p
            case mmatch of
                Nothing     -> return Nothing
                Just (i,j)  -> return (Just (p+i,p+j))    -- offset from point

------------------------------------------------------------------------

-- | calculate whether a move is in bounds.
inBounds :: Int -> Int -> Int
inBounds i end | i <= 0    = 0
               | i >= end  = max 0 (end - 1)
               | otherwise = i
{-# INLINE inBounds #-}

-- ---------------------------------------------------------------------
-- General undo/redo support. Based on proposal by sjw.
--
-- Big deletes will be a problem at the moment.
--

--
-- | A URList consists of an undo and a redo list.
--
data URList = URList ![URAction] ![URAction]
--  deriving Show

--
-- Mutation actions (from the undo or redo list) are either inserts or
-- deletions
--
data URAction = Insert !Int !Char
              | Delete !Int
--  deriving Show

-- ---------------------------------------------------------------------
-- | Create a new 'URList'.
emptyUR :: URList
emptyUR = URList [] []

--
-- | Add an action to the undo list. The first argument is either a
-- @Left@ buffer point and 'Char' to insert, or @Right@, a point from
-- which to delete a character.
addUR :: URList -> URAction -> URList
addUR (URList us rs) u = URList (u:us) rs

--
-- | Undo the last action that mutated the buffer contents. The action's
-- inverse is added to the redo list.
--
undoUR :: FBuffer -> URList -> IO URList
undoUR _ u@(URList [] _) = return u
undoUR b (URList (u:us) rs) = do
    let f = getAction u
    r <- f b
    return (URList us (r:rs))

--
-- | Redo the last action that mutated the buffer contents. The action's
-- inverse is added to the undo list.
--
redoUR :: FBuffer -> URList -> IO URList
redoUR _ u@(URList _ []) = return u
redoUR buf (URList us (r:rs)) = do
    u <- (getAction r) buf
    return (URList (u:us) rs)

-- ---------------------------------------------------------------------
-- INTERNAL:
--
-- Given a URAction, return the buffer action it represents, and the 
-- URAction that reverses it.
--
getAction :: URAction -> (FBuffer -> IO URAction) 
getAction (Delete p) b = do 
    moveTo b p
    c <- readB b
    deleteN' b 1       -- need to be actions that don't in turn invoke the url
    return (Insert p c)

getAction (Insert p c) b = do
    moveTo b p
    insertN' b [c] 1     -- hmm. will modify the ur list
    return (Delete p)

