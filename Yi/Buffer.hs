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

    -- | Length of contents of buffer
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
       ,bufEnd   :: !Int        -- ^ pointer to end of chars
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
    b@(FBuffer_ { bufBuf=buf }) <- allocateFBuffer_ (2*sz)
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
    let loop j
           | j >= n    = return []
           | otherwise = do
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

{-
--
-- | Find the offset from the current point back to the nearest '\n' or
-- start of file. (find the offset from the previous new line)
--
prevNLOff :: RawBuffer -> Int -> IO Int
prevNLOff buf i = do
    j <- loop i
    return (i - j)
  where
    loop k | k <= 0    = return 0
           | otherwise = do (c,_) <- readCharFromBuffer buf k
                            if c == '\n' then return k
                                         else loop $! k - 1

nextNLOff :: RawBuffer -> Int -> Int -> IO Int
nextNLOff buf max i = assert (i <= max) $ do j <- loop i ; return (j - i)
  where
    loop k | k >= max  = return (max - 1)
           | otherwise = do (c,_) <- readCharFromBuffer buf k
                            if c == '\n' then return k
                                         else loop $! k + 1
-}

--
-- | calculate whether a move is in bounds. not threadsave, so only call
-- from with withMVar.
--
rawMoveTo :: Int -> FBuffer_ -> FBuffer_
rawMoveTo i b@(FBuffer_ {bufEnd=j}) =
        if i < 0 then b{bufPnt=0} 
                 else if i >= j then b{bufPnt=j-1}
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
            readIORef >>= \(FBuffer_ b _ n _) ->
                readChars b n 0

    -- size :: FBuffer -> IO Int
    size  (FBuffer _ _ m) =
        readMVar m >>=
            readIORef >>= \(FBuffer_ _ _ i _) -> return i

    -- point :: FBuffer -> IO Int
    point (FBuffer _ _ m) =
        readMVar m >>=
            readIORef >>= \(FBuffer_ _ p _ _) -> return p

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
            i <- writeChars buf [c] p
            writeIORef ref (b{bufPnt = i})
            return fb
                
        
-- ---------------------------------------------------------------------
-- | 2-D primitives implemented over the 'Buffer' primitives
--
-- Clean these up a bit
--

--
-- | Move to previous \n, or start of file, and return the modified
-- buffer
--
prevNL :: Buffer a => a -> IO a
prevNL b = do
    let loop 0# = return ()
        loop p  = do c <- char b
                     when (c /= '\n') $ 
                            left b >> loop (p -# 1#)
    (I# p) <- point b
    loop p
    return b

--
-- | Return the offset to the previous \n -- don't move there
--
prevNLOffset_ :: Buffer a => a -> IO Int
prevNLOffset_ b = do
    p <- point b
    x <- prevNLOffset b
    moveTo p b
    return x

--
-- | Return the index of the previous \n, or start of file
--
prevNLIx :: Buffer a => a -> IO Int
prevNLIx b = do
    p <- point b
    prevNLOffset b
    q <- point b
    moveTo p b
    return q
        
        
        

--
-- | Return the offset to the previous \n , and move there
--
prevNLOffset :: Buffer a => a -> IO Int
prevNLOffset b = do
    let loop acc 0# = return (I# acc)
        loop acc p  = do left b
                         c <- char b
                         if c == '\n' 
                            then return (I# acc)
                            else loop (acc +# 1#) (p -# 1#)
    (I# p) <- point b
    loop 0# p

--
-- | Move to next '\n', returning the buffer. Stay put if we're on a \n.
--
nextNL :: Buffer a => a -> IO a
nextNL b = do
    (I# e') <- size b
    let e = e' -# 1#
        loop p | p  ==# e   = return () -- end of buffer
               | otherwise  = do c <- char b
                                 when (c /= '\n') $ do
                                    right b
                                    loop (p +# 1#)
    (I# p) <- point b
    loop p
    return b

--
-- | Move forward @x@, or end of line, or end of file, 
-- Return the buffer
--
nextXorNL :: Buffer a => a -> Int -> IO a
nextXorNL b (I# x) = do
    (I# e') <- size  b
    let e   = e' -# 1#
        loop acc p | p   ==# e  = return (I# acc)   -- end of file
                   | acc ==# x  = return (I# acc)   -- moved x chars
                   | otherwise  = do right b
                                     c <- char b
                                     if c == '\n'
                                        then return (I# acc)
                                        else loop (acc +# 1#) (p +# 1#)
    (I# p) <- point b
    loop 0# p
    return b

