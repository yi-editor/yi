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
-- | This module defines operations on a one-dimension buffer. Instances
-- of 'Buffer' are available.
--

module Yi.Buffer where

import Data.IORef
import Data.Unique
import Control.Monad            ( liftM, when )
import Control.Concurrent.MVar

import GHC.Base
import GHC.IOBase   ( IO(..) )
import System.IO    hiding ( IO )
import Prelude      hiding ( IO )

--
-- | The 'Buffer' defines operations over one-dimensional buffers,
-- providing basic editing operations.
--
class Buffer a where

    -- | A buffer has an (immutable) name
    name       :: a -> String

    -- | A buffer has an immutable unique key
    key        :: a -> Unique

    -- | New buffer filled from 'FilePath'. Buffers know how to fill themselves.
    newBuffer  :: FilePath -> IO a

    --
    -- | Get the buffer contents in a useful form: '[Char]'
    --
    contents   :: a -> IO [Char]

    -- | Number of characters in the buffer, or index of last elem+1
    size       :: a -> IO Int

    -- | Current position of the cursor. Offset from start of buffer
    point      :: a -> IO Int

    -- | Character at the current point
    char       :: a -> IO Char

    -- | Move cursor to arbitrary point. If the point overruns in any
    -- direction, the cursor is moved to the limit in that dimension
    -- Does some IO and returns the new buffer
    moveTo      :: Int -> a -> IO a

    -- | Move cursor -n
    leftN       :: Int -> a -> IO a
    leftN n b   = point b >>= \p -> moveTo (p - n) b

    -- | Move cursor -1
    left        :: a -> IO a
    left        = leftN 1

    -- | Move cursor +n
    rightN      :: Int -> a -> IO a
    rightN n b   = point b >>= \p -> moveTo (p + n) b

    -- | Move cursor +1
    right       :: a -> IO a
    right       = rightN 1

    -- | Overwrite contents of buffer from point onwards with string
    replace     :: Char -> a -> IO a

    -- | Insert character at point, shifting along rest of buffer
    insert      :: Char -> a -> IO a

    -- | Delete character at point, shifting back rest of buffer
    delete      :: a -> IO a
    
    -- | Delete @n@ characters forward from point, or till eof
    deleteN     :: Int -> a -> IO a

-- ---------------------------------------------------------------------
--
-- Fast buffer based on GHC.IORef.{Handle,IOBase} (Handle)
--

data FBuffer = 
    FBuffer 
        FilePath{-immutable-} 
        Unique  {-immutable-}
        !(MVar (IORef FBuffer_))

data FBuffer_ = FBuffer_ {
        bufBuf   :: RawBuffer   -- ^ a raw byte buffer
       ,bufPnt   :: !Int        -- ^ current pointer index
       ,bufEnd   :: !Int        -- ^ index to end of chars
       ,bufSize  :: !Int        -- ^ max size
     }

type RawBuffer = MutableByteArray# RealWorld

--
-- | Buffers are equal if they have the same unique.
--
instance Eq FBuffer where
   (FBuffer _ u _) == (FBuffer _ v _)     = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer f _ _) = showString $ "\"" ++ f ++ "\""

-- ---------------------------------------------------------------------
-- | Initialisation stuff.
--

--
-- | A new 'FBuffer_'
--
newEmptyFBuffer_ :: RawBuffer -> Int -> FBuffer_
newEmptyFBuffer_ b sz = 
    FBuffer_ { bufBuf = b, 
               bufPnt = 0, -- work out how to deal with empty files
               bufEnd = 0,
               bufSize = sz }

--
-- | Creates a new mutable byte array (a 'FBuffer_') of specified size
-- (in bytes)
--
allocateFBuffer_ :: Int -> IO FBuffer_
allocateFBuffer_ sz@(I# n) = IO $ \st ->
    case newByteArray# n st of 
        (# st', b #) -> (# st', newEmptyFBuffer_ b sz #)

--
-- | get a new 'FBuffer_', 2 * size of @Handle@, by default, filled from
-- @Handle@.
--
fillNewFBuffer_ :: Handle -> IO (IORef FBuffer_)
fillNewFBuffer_ hd = do
    sz <- liftM fromIntegral (hFileSize hd)
    ss <- hGetContents hd    -- race. could have shrunk.
    b@(FBuffer_ { bufBuf=buf }) <- allocateFBuffer_ (sz+2048)
    i <- writeChars buf ss 0
    newIORef (b { bufEnd = i })

--
-- | Fill up a new 'FBuffer' using @f@.
--
newFBuffer :: FilePath -> IO FBuffer
newFBuffer f = do
    hd   <- openFile f ReadMode
    buf  <- fillNewFBuffer_ hd      -- think about resizing
    hClose hd
    mbuf <- newMVar buf
    u    <- newUnique
    return (FBuffer f u mbuf)

-- ---------------------------------------------------------------------
--
-- | could use read(), mmap.. or something like that, though this is cute.
-- Return the index of the next point
--
writeChars :: RawBuffer -> [Char] -> Int -> IO Int
writeChars _ []     i = return i 
writeChars b (c:cs) i = writeCharToBuffer b i c >>= writeChars b cs 

--
-- | read @n@ chars from buffer @b@, starting at @i@
--
readChars :: RawBuffer -> Int -> Int -> IO [Char]
readChars b n i = do
    let loop j | j >= (i + n) = return []
               | otherwise    = do
                    (c,_) <- readCharFromBuffer b j
                    cs    <- loop $! j+1
                    return (c : cs)
    loop i

--
-- | Write a 'Char' into a 'RawBuffer', 
-- Stolen straight from GHC. (Write 8-bit character; offset in bytes)
-- Return new offset.
--
-- TODO ** we're screwed if the buffer is too small, so realloc
-- Will have to resize if off >= size
--
writeCharToBuffer :: RawBuffer -> Int -> Char -> IO Int
writeCharToBuffer slab (I# off) (C# c) = IO $ \st -> 
    case writeCharArray# slab off c st of
             st' -> (# st', I# (off +# 1#) #)

{-
--
-- | resize buffer if we are near the end.
--
safeWriteCharToBuffer :: RawBuffer -> Int -> Int -> Char -> IO Int
safeWriteCharToBuffer slab size off c
    | off > size - 1 = do slab' <- realloc slab size
                          writeCharToBuffer slab off c
    | otherwise      = writeCharToBuffer slab off c
-}

--
-- | Read a 'Char' from a 'RawBuffer'. Return character, and inc new offset.
--
readCharFromBuffer :: RawBuffer -> Int -> IO (Char, Int)
readCharFromBuffer slab (I# off) = IO $ \st -> 
    case readCharArray# slab off st of
             (# st', c #) -> (# st', (C# c, I# (off +# 1#)) #)

--
-- | calculate whether a move is in bounds. not threadsafe, so only call
-- from with withMVar.
--
rawMoveTo :: Int -> FBuffer_ -> FBuffer_
rawMoveTo i b@(FBuffer_ {bufEnd=end}) =
        if i < 0 then b{bufPnt=0} 
                 else if i >= end then b{bufPnt=end-1}
                                  else b{bufPnt=i}

-- ---------------------------------------------------------------------
--
-- | 'FBuffer' is a member of the 'Buffer' class, providing fast
-- indexing operations. It is implemented in terms of a mutable byte
-- array.
--

instance Buffer FBuffer where

    -- --------------------------------------------------------------------- 
    -- "stuff what looks at the buffer"

    -- name :: FBuffer -> String
    name (FBuffer f _ _) = f

    -- key :: FBuffer -> Unique
    key (FBuffer _ u _)  = u

    -- newBuffer :: FilePath -> IO FBuffer
    newBuffer = newFBuffer

    -- contents  :: FBuffer -> [Char]
    contents (FBuffer _ _ m) =
        readMVar m >>= 
            readIORef >>= \(FBuffer_ {bufBuf=b,bufEnd=n}) ->
                readChars b n 0

    -- size :: FBuffer -> IO Int
    size (FBuffer _ _ m) =
        readMVar m >>=
            readIORef >>= \(FBuffer_ {bufEnd=n}) -> return n

    -- point :: FBuffer -> IO Int
    point (FBuffer _ _ m) =
        readMVar m >>=
            readIORef >>= \(FBuffer_ {bufPnt=p}) -> return p

    -- char :: FBuffer -> IO Char
    char  (FBuffer _ _ m) =
        readMVar m >>=
            readIORef >>= \(FBuffer_ b p _ _) ->
                readCharFromBuffer b p >>= \(c,_) -> return c

    -- ---------------------------------------------------------------------
    -- "stuff what messes with the buffer"

    -- moveTo :: Int -> FBuffer -> IO FBuffer{-modified-}
    moveTo i fb@(FBuffer _ _ m) =
        withMVar m $ \ref -> do   -- return result of action
            b <- readIORef ref
            let b' = rawMoveTo i b
            writeIORef ref b'
            return fb

    -- replace  :: Char -> a -> IO a
    replace c fb@(FBuffer _ _ m) = 
        withMVar m $ \ref -> do
            b@(FBuffer_ {bufBuf=buf,bufPnt=p}) <- readIORef ref        
            writeChars buf [c] p
            writeIORef ref b
            return fb

    -- insert :: Char -> a -> IO a
    insert c fb@(FBuffer _ _ m) =
        withMVar m $ \ref -> do
            b@(FBuffer_ buf p e _) <- readIORef ref        
            cs <- readChars buf  (e-p) p
            i  <- writeChars buf (c:cs)  p
            writeIORef ref (b{bufPnt=p+1,bufEnd=i}) -- shift point
            return fb

     -- delete :: a -> IO a
    delete = deleteN 1

     -- delete :: Int -> a -> IO a
    deleteN n fb@(FBuffer _ _ m) =
        withMVar m $ \ref -> do
            b@(FBuffer_ buf p e _) <- readIORef ref        
            cs <- readChars buf (e-p-n) (p+n)
            i  <- writeChars buf cs p  -- index of last char written
            let p' | p == 0    = p     -- go no further!
                   | i == p    = p-1   -- shift back if at eof
                   | otherwise = p
            writeIORef ref (b{bufPnt=p',bufEnd=i})
            return fb
        
-- ---------------------------------------------------------------------
-- | "2-D" primitives implemented over the 'Buffer' primitives
-- Needed for 2-D guis.

--
-- | Move to the char /in front of/ the prev \n, or the start of file.
-- If the current char is a \n, we are already there.
--
gotoPrevLn :: Buffer a => a -> IO a
gotoPrevLn b = do
    let loop 0# = return ()
        loop p  = do c <- char b
                     if (c == '\n') then right b >> return ()
                                    else left  b >> loop (p -# 1#)
    (I# p) <- point b
    loop (p +# 1#)
    return b

--
-- | Move to the char after the next '\n'. If the current char is a \n,
-- this means move right 1.
--
gotoNextLn :: Buffer a => a -> IO a
gotoNextLn b = do
    (I# e') <- size b
    let e = e' -# 1#
        loop p | p ==# e   = return () -- end of buffer
               | otherwise = do c <- char b
                                right b
                                when (c /= '\n') $ loop (p +# 1#)
    (I# p) <- point b
    loop p
    return b

------------------------------------------------------------------------

--
-- | Return the /index/ of the start of line char, or the last \n if
-- we're at the end of file, and that char in a \n.
--
prevLnIx :: Buffer a => a -> IO Int
prevLnIx b = do
    p <- point b
    gotoPrevLn b
    i <- point b
    moveTo p b
    return i

--
-- | Return the /index/ to the start of the next line, or what is the
-- index of the char after the next \n?). Index of eof+1 if last line
--
nextLnIx :: Buffer a => a -> IO Int
nextLnIx b = do
    p <- point b
    gotoNextLn b
    i <- point b
    s <- size  b
    moveTo p   b
    return $ if s-1 == i then i+1 else i -- next nl past eof (safe, but good?)

--
-- | Return the offset (distance to shift) to the start of the previous
-- line. If I'm currently on a \n, this means finding the char in front
-- of the last \n
--
prevLnOffset :: Buffer a => a -> IO Int
prevLnOffset b = do
    j <- point b
    left b  -- hop over any \n
    i <- prevLnIx b
    moveTo j b
    return (j - i)

--
-- | Return the offset to the next line. (the char after the next \n)
-- So if we're at the start of a line, give the offset to the next line
--
nextLnOffset :: Buffer a => a -> IO Int
nextLnOffset b = do
    i <- point b
    j <- nextLnIx b
    return (j - i - 1)

--
-- | Move forward @x@, or end of line, or end of file, 
-- Return the buffer
--
nextXorNL :: Buffer a => Int -> a -> IO a
nextXorNL x b = do
    n <- nextLnOffset b
    rightN (min x n) b

--
-- | Move back @x@, or to the start of the line.
--
prevXorLn :: Buffer a => Int -> a -> IO a
prevXorLn x b = do
    n <- prevLnOffset b
    leftN (min x n) b

--
-- | Kill all chars to end of line, not including the \n
-- What happens if there is no \n on this line?
-- EOF with no \n sucks.
--
killToNL :: Buffer a => a -> IO a
killToNL b = do
    x <- nextLnOffset b
    deleteN x b

