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
-- | An interface to a one dimensional mutable 'Buffer', providing
-- cursor movement and editing commands
--

module Yi.Buffer where

import Yi.Regex                 ( Regex  )
import Control.Monad            ( when )
import Yi.FastBuffer
import Yi.Undo

import Yi.Debug
import Data.IORef
import Data.Unique              ( newUnique, Unique )
import Control.Concurrent.MVar


--
-- | The 'Buffer' class defines editing operations over one-dimensional`
-- mutable buffers, which maintain a current /point/.
--

type Point = Yi.FastBuffer.Point --  to re-export.

data BufferMode = ReadOnly | ReadWrite

data FBuffer =
        FBuffer { name   :: !String           -- ^ immutable buffer name
                , bkey   :: !Unique           -- ^ immutable unique key
                , file   :: !(MVar (Maybe FilePath)) -- ^ maybe a filename associated with this buffer
                , undos  :: !(MVar URList)      -- ^ undo/redo list
                , rawbuf :: !BufferImpl
                , bmode  :: !(MVar BufferMode)  -- ^ a read-only bit
                , preferCol :: !(IORef (Maybe Int))      -- ^ prefered column to arrive at when we do a lineDown / lineUp
                }

instance Eq FBuffer where
   FBuffer { bkey = u } == FBuffer { bkey = v } = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer { name = f }) = showString $ "\"" ++ f ++ "\""

lift :: (BufferImpl -> x) -> (FBuffer -> x)
lift f = \b -> f (rawbuf b)

hNewB :: FilePath -> IO FBuffer
hNewB nm = do s <- readFile nm
              b <- newB nm s
              setfileB b nm
              return b

hPutB :: FBuffer -> FilePath -> IO ()
hPutB b nm = elemsB b >>= writeFile nm

nameB :: FBuffer -> String
nameB (FBuffer { name = n }) = n

getfileB :: FBuffer -> IO (Maybe FilePath)
getfileB (FBuffer { file = mvf }) = readMVar mvf

setfileB :: FBuffer -> FilePath -> IO ()
setfileB (FBuffer { file = mvf }) f =
    modifyMVar_ mvf $ const $ return (Just f)

keyB :: FBuffer -> Unique
keyB (FBuffer { bkey = u }) = u


isUnchangedB  :: FBuffer -> IO Bool
isUnchangedB (FBuffer { undos = mv }) = do
  ur <- readMVar mv
  return $ isEmptyUList ur

undo        :: FBuffer -> IO ()
undo fb@(FBuffer { undos = mv }) = modifyMVar_ mv (undoUR (rawbuf fb))

redo        :: FBuffer -> IO ()
redo fb@(FBuffer { undos = mv }) = modifyMVar_ mv (redoUR (rawbuf fb))

-- | Create buffer named @nm@ with contents @s@
newB :: String -> [Char] -> IO FBuffer
newB nm s = do 
    pc <- newIORef Nothing
    mv <- newBI s
    mv' <- newMVar emptyUR
    mvf <- newMVar Nothing      -- has name, not connected to a file
    rw  <- newMVar ReadWrite
    u   <- newUnique
    return $ FBuffer { name   = nm
                     , bkey   = u
                     , file   = mvf
                     , undos  = mv'
                     , rawbuf = mv
                     , bmode  = rw
                     , preferCol = pc 
                     }
                     

-- | Free any resources associated with this buffer
finaliseB :: FBuffer -> IO ()
finaliseB = lift finaliseBI

-- | Number of characters in the buffer
sizeB     :: FBuffer -> IO Int
sizeB = lift sizeBI

-- | Extract the current point
pointB    :: FBuffer -> IO Int
pointB = lift pointBI

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsB   :: FBuffer -> Int -> Int -> IO [Char]
nelemsB = lift nelemsBI

------------------------------------------------------------------------
-- Point based operations

-- | Move point in buffer to the given index
moveTo    :: FBuffer -> Int -> IO ()
moveTo b x = do 
  forgetPerferCol b  
  lift moveToI b x

------------------------------------------------------------------------

-- | Write an element into the buffer at the current point
-- This is an unsafe operation, no bounds checks are performed
-- TODO: undo is not atomic!
writeB    :: FBuffer -> Char -> IO ()
writeB b@FBuffer { undos = uv } c = do
  forgetPerferCol b
  off <- pointB b
  oldc <- nelemsB b 1 off
  modifyMVar_ uv $ \u -> do
    let u'  = addUR u  (Insert off oldc)
        u'' = addUR u' (Delete off 1)
    return u''
  writeBI (rawbuf b) c

------------------------------------------------------------------------

-- | Insert the list at current point, extending size of buffer
insertN   :: FBuffer -> [Char] -> IO ()
insertN  _ [] = return ()
insertN fb@FBuffer { undos = uv } cs = do
  forgetPerferCol fb
  pnt <- pointB fb
  modifyMVar_ uv $ \ur -> return $ addUR ur (Delete pnt (length cs))
  insertNI (rawbuf fb) cs

------------------------------------------------------------------------

-- | @deleteNAt b n p@ deletes @n@ characters at position @p@
deleteNAt:: FBuffer -> Int -> Int -> IO ()
deleteNAt _ 0 _ = return ()
deleteNAt b@FBuffer { undos = uv }  n pos = 
    do forgetPerferCol b
       text <- nelemsB b n pos
       modifyMVar_ uv $ \ur -> return $ addUR ur (Insert pos text)
       lift deleteNAtI b n pos

------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLn      :: FBuffer -> IO Int
curLn = lift curLnI 

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLn     :: FBuffer -> Int -> IO Int
gotoLn = lift gotoLnI

---------------------------------------------------------------------

-- | Return index of next string in buffer that matches argument
searchB     :: FBuffer -> [Char] -> IO (Maybe Int)
searchB = lift searchBI

-- | Set name of syntax highlighting mode
setSyntaxB :: FBuffer -> [Char] -> IO ()
setSyntaxB = lift setSyntaxBI

-- | Return indices of next string in buffer matched by regex
regexB      :: FBuffer -> Regex -> IO (Maybe (Int,Int))
regexB = lift regexBI

---------------------------------------------------------------------

-- | Set this buffer mark (TODO: have a set of these (bookmarks, error list, etc.))
setMarkB       :: FBuffer -> Int -> IO ()
setMarkB = lift setMarkBI

getMarkB       :: FBuffer -> IO Int
getMarkB = lift getMarkBI

unsetMarkB     :: FBuffer -> IO ()
unsetMarkB = lift unsetMarkBI

-- | Move point -1
leftB       :: FBuffer -> IO ()
leftB a     = leftN a 1

-- | Move cursor -n
leftN       :: FBuffer -> Int -> IO ()
leftN a n   = pointB a >>= \p -> moveTo a (p - n)

-- | Move cursor +1
rightB      :: FBuffer -> IO ()
rightB a    = rightN a 1

-- | Move cursor +n
rightN      :: FBuffer -> Int -> IO ()
rightN a n = pointB a >>= \p -> moveTo a (p + n)

-- ---------------------------------------------------------------------
-- Line based movement and friends


-- | Move point down by @n@ lines. @n@ can be negative.
lineMoveRel :: FBuffer -> Int -> IO ()
lineMoveRel b n = do
  prefCol <- readIORef (preferCol b)
  targetCol <- case prefCol of
    Nothing -> offsetFromSol b
    Just x -> return x
  --logPutStrLn $ "lineMoveRel: targetCol = " ++ show targetCol
  gotoLnFrom b n
  moveXorEol b targetCol
  writeIORef (preferCol b) (Just targetCol)

forgetPerferCol :: FBuffer -> IO ()
forgetPerferCol b = do
  --logPutStrLn "forgetPerferCol"
  writeIORef (preferCol b) Nothing

-- | Move point up one line
lineUp :: FBuffer -> IO ()
lineUp b = lineMoveRel b (-1)

-- | Move point down one line
lineDown :: FBuffer -> IO ()
lineDown b = lineMoveRel b 1


-- | Return the contents of the buffer as a list
elemsB ::  FBuffer -> IO [Char]
elemsB b = do n <- sizeB b
              nelemsB b n 0

-- | Read the character at the current point
readB :: FBuffer -> IO Char
readB b = pointB b >>= readAtB b

-- | Read the character at the given index
-- This is an unsafe operation: character NUL is returned when out of bounds
readAtB :: FBuffer -> Int -> IO Char
readAtB b i = do
    s <- nelemsB b 1 i
    return $ case s of
               [c] -> c
               _ -> '\0'

-- | Delete the character at current point, shrinking size of buffer
deleteB :: FBuffer -> IO ()
deleteB a = deleteN a 1

-- | Delete @n@ characters forward from the current point
deleteN :: FBuffer -> Int -> IO ()
deleteN _ 0 = return ()
deleteN b n = do
  point <- pointB b
  deleteNAt b n point

-- | Delete to the end of line, excluding it.
deleteToEol :: FBuffer -> IO ()
deleteToEol b = do
    p <- pointB b
    moveToEol b
    q <- pointB b
    deleteNAt b (q-p) p



------------------------------------------------------------------------

-- | Return true if the current point is the start of a line
atSol :: FBuffer -> IO Bool
atSol a = do p <- pointB a
             if p == 0 then return True
                       else do c <- readAtB a (p-1)
                               return (c == '\n')

-- | Return true if the current point is the end of a line
atEol :: FBuffer -> IO Bool
atEol a = do p <- pointB a
             e <- sizeB a
             if p == e
                    then return True
                    else do c <- readAtB a p
                            return (c == '\n')

-- | True if point at start of file
atSof :: FBuffer -> IO Bool
atSof a = do p <- pointB a
             return (p == 0)

-- | True if point at end of file
atEof :: FBuffer -> IO Bool
atEof a = do p <- pointB a
             e <- sizeB a
             return (p == e)


-- | Offset from start of line
offsetFromSol :: FBuffer -> IO Int
offsetFromSol a = do
    i <- pointB a
    moveToSol a
    j <- pointB a
    moveTo a i
    return (i - j)
{-# INLINE offsetFromSol #-}

-- | Index of start of line
indexOfSol :: FBuffer -> IO Int
indexOfSol a = do
    i <- pointB a
    j <- offsetFromSol a
    return (i - j)
{-# INLINE indexOfSol #-}

-- | Index of end of line
indexOfEol :: FBuffer -> IO Int
indexOfEol a = do
    i <- pointB a
    moveToEol a
    j <- pointB a
    moveTo a i
    return j
{-# INLINE indexOfEol #-}


-- | Move using the direction specified by the 2nd argument, until
-- either we've moved @n@, the 3rd argument, or @p@ the 4th argument
-- is True
moveAXuntil :: FBuffer -> (FBuffer -> IO ()) -> Int -> (FBuffer -> IO Bool) -> IO ()
moveAXuntil b f x p
    | x <= 0    = return ()
    | otherwise = do -- will be slow on long lines...
        let loop 0 = return ()
            loop i = do r <- p b
                        when (not r) $ f b >> loop (i-1)
        loop x
{-# INLINE moveAXuntil #-}

-- | Move @x@ chars back, or to the sol, whichever is less
moveXorSol  :: FBuffer -> Int -> IO ()
moveXorSol a x = moveAXuntil a leftB x atSol

-- | Move @x@ chars forward, or to the eol, whichever is less
moveXorEol  :: FBuffer -> Int -> IO ()
moveXorEol a x = moveAXuntil a rightB x atEol


-- | Go to line indexed from current point
gotoLnFrom :: FBuffer -> Int -> IO Int
gotoLnFrom b x = do 
  l <- curLn b 
  gotoLn b (x+l)
  return (x+l)


-- | Move point to start of line
moveToSol :: FBuffer -> IO ()
moveToSol b = sizeB b >>= moveXorSol b  

-- | Move point to end of line
moveToEol :: FBuffer -> IO ()
moveToEol b = sizeB b >>= moveXorEol b 

