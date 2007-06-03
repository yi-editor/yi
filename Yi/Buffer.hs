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

module Yi.Buffer ( FBuffer (..), BufferM, runBuffer, keyB, curLn, nameB, indexOfEol,
                   sizeB, pointB, moveToSol, moveTo, lineUp, lineDown,
                   hPutB, hNewB, newB, finaliseB, Point, Mark, BufferMode(..),
                   moveToEol, gotoLn, gotoLnFrom, offsetFromSol,
                   atSol, atEol, atSof, atEof, leftB, rightB,
                   moveXorEol, moveXorSol, insertN, deleteN,
                   deleteToEol, indexOfSol, nelemsB, writeB, getfileB,
                   setfileB, deleteNAt, readB, elemsB, undo, redo,
                   getMarkB, getSelectionMarkB, getMarkPointB, setMarkPointB, unsetMarkB, 
                   isUnchangedB, setSyntaxB, regexB, searchB, readAtB,
                   getModeLine, getPercent, forgetPreferCol, setBufferKeymap, restartBufferThread,
                   clearUndosB, addOverlayB,
                   getDynamicB, setDynamicB
                    ) where

import Prelude hiding ( error )
import System.FilePath
import Text.Regex.Posix.Wrap    ( Regex  )
import Yi.FastBuffer
import Yi.Undo
import Yi.Style

import Yi.Debug
import Yi.Dynamic
import Data.IORef
import Data.Unique              ( newUnique, Unique, hashUnique )
import Yi.Event
import Yi.Keymap
import Control.Concurrent 
import Control.Monad
import Control.Monad.Trans
import Control.Monad.RWS
import Control.Exception

--
-- | The 'Buffer' class defines editing operations over one-dimensional`
-- mutable buffers, which maintain a current /point/.
--

data BufferMode = ReadOnly | ReadWrite

data FBuffer =
        FBuffer { name   :: !String                  -- ^ immutable buffer name
                , bkey   :: !Unique                  -- ^ immutable unique key
                , file   :: !(MVar (Maybe FilePath)) -- ^ maybe a filename associated with this buffer
                , undos  :: !(MVar URList)           -- ^ undo/redo list
                , rawbuf :: !BufferImpl
                , bmode  :: !(MVar BufferMode)       -- ^ a read-only bit
                , bufferDynamic :: !(IORef DynamicValues) -- ^ dynamic components
                , preferCol :: !(IORef (Maybe Int))  -- ^ prefered column to arrive at when we do a lineDown / lineUp
                , bufferInput  :: !(Chan Event)      -- ^ input stream
                , bufferThread :: !(Maybe ThreadId)  -- ^ Id of the thread running the buffer's keymap. 
                , bufferKeymap :: !(IORef KeymapMod) -- ^ Buffer's local keymap modification
                , bufferKeymapRestartable :: !(MVar ()) -- ^ Put () in this MVar to mark the buffer ready to restart.
                                             -- FIXME: the bufferKeymap should really be an MVar, and that can be used to sync.
                }

type BufferM a = RWST FBuffer [URAction] () IO a

instance Eq FBuffer where
   FBuffer { bkey = u } == FBuffer { bkey = v } = u == v

instance Show FBuffer where
    showsPrec _ (FBuffer { bkey = u, name = f, bufferThread = t }) = 
        showString $ "Buffer #" ++ show (hashUnique u) ++ "/" ++ show t ++ " (" ++ show f ++ ")"

-- | Given a buffer, and some information update the modeline
--
-- N.B. the contents of modelines should be specified by user, and
-- not hardcoded.
--
getModeLine :: BufferM String
getModeLine = do
    col <- offsetFromSol
    pos <- pointB
    ln <- curLn
    p <- indexOfEol
    s <- sizeB
    unchanged <- isUnchangedB
    let pct = if pos == 1 then "Top" else getPercent p s
        chg = if unchanged then "-" else "*"
    nm <- nameB
    return $ 
           chg ++ " "
           ++ nm ++ 
           replicate 5 ' ' ++
           "L" ++ show ln ++ "  " ++ "C" ++ show col ++ 
           replicate 2 ' ' ++ pct

--
-- | Give a point, and the file size, gives us a percent string
--
getPercent :: Int -> Int -> String
getPercent a b = show p ++ "%"
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int

withImpl :: (BufferImpl -> IO x) -> (BufferM x)
withImpl f = do b <- ask; lift $ f (rawbuf b)

addOverlayB :: Point -> Point -> Style -> BufferM ()
addOverlayB s e sty = withImpl $ addOverlayBI s e sty

runBuffer :: FBuffer -> BufferM a -> IO (a, [URAction])
runBuffer b f = do 
  (a, (), ur) <- runRWST f b ()
  return (a, ur)

hNewB :: FilePath -> IO FBuffer
hNewB fp = do
  s <- readFile fp
  b <- newB (takeFileName fp) s -- FIXME: Here we should somehow insure that no 2 buffers get the same name
  runBuffer b (setfileB fp)
  return b

hPutB :: BufferM ()
hPutB = do
  mf <- getfileB
  case mf of
    Nothing -> error "buffer not associated with a file"
    Just f  -> lift . writeFile f =<< elemsB
  clearUndosB


clearUndosB :: BufferM ()
clearUndosB = do b <- ask; lift $ modifyMVar_ (undos b) (\_ -> return emptyUR) -- Clear the undo list, so the changed "flag" is reset.

nameB :: BufferM String
nameB = asks name

getfileB :: BufferM (Maybe FilePath)
getfileB = do (FBuffer { file = mvf }) <- ask; lift $ readMVar mvf

setfileB :: FilePath -> BufferM ()
setfileB f = do (FBuffer { file = mvf }) <- ask; lift $ modifyMVar_ mvf $ const $ return (Just f)

keyB :: FBuffer -> Unique
keyB (FBuffer { bkey = u }) = u


isUnchangedB :: BufferM Bool
isUnchangedB = do
  b <- ask
  ur <- lift $ readMVar $ undos b
  return $ isEmptyUList ur

undo :: BufferM ()
undo = do fb@(FBuffer { undos = mv }) <- ask; u <- lift $ modifyMVar mv (undoUR (rawbuf fb)); tell u

redo :: BufferM ()
redo = do fb@(FBuffer { undos = mv }) <- ask; u <- lift $ modifyMVar mv (redoUR (rawbuf fb)); tell u

-- | Create buffer named @nm@ with contents @s@
newB :: String -> [Char] -> IO FBuffer
newB nm s = do 
    pc <- newIORef Nothing
    mv <- newBI s
    mv' <- newMVar emptyUR
    mvf <- newMVar Nothing      -- has name, not connected to a file
    rw  <- newMVar ReadWrite
    u   <- newUnique
    ch  <- newChan
    km <- newIORef id
    r <- newEmptyMVar
    dv <- newIORef emptyDV
    let result = FBuffer { name   = nm
                         , bkey   = u
                         , file   = mvf
                         , undos  = mv'
                         , rawbuf = mv
                         , bmode  = rw
                         , preferCol = pc 
                         , bufferDynamic = dv
                         , bufferInput = ch
                         , bufferThread = Nothing
                         , bufferKeymap = km
                         , bufferKeymapRestartable = r
                         }
    return result                    


setBufferKeymap :: FBuffer -> KeymapMod -> IO ()
setBufferKeymap b km = do 
  writeIORef (bufferKeymap b) km
  restartBufferThread b
  logPutStrLn $ "Changed keymap for buffer " ++ show b

restartBufferThread :: FBuffer -> IO ()
restartBufferThread b = do
  logPutStrLn $ "Waiting for buffer thread to start " ++ show b
  takeMVar (bufferKeymapRestartable b) 
  maybe (return ()) (flip throwDynTo "Keymap change") (bufferThread b)

-- | Free any resources associated with this buffer
finaliseB :: BufferM ()
finaliseB = do 
  b <- ask; lift $ maybe (return ()) killThread (bufferThread b)
  withImpl finaliseBI

-- | Number of characters in the buffer
sizeB :: BufferM Int
sizeB = withImpl sizeBI

-- | Extract the current point
pointB :: BufferM Int
pointB = withImpl pointBI

-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsB :: Int -> Int -> BufferM [Char]
nelemsB n i = withImpl $ nelemsBI n i

------------------------------------------------------------------------
-- Point based operations

-- | Move point in buffer to the given index
moveTo :: Int -> BufferM ()
moveTo x = do 
  forgetPreferCol
  withImpl $ moveToI x

------------------------------------------------------------------------

applyUpdate :: URAction -> BufferM ()
applyUpdate update = do
  forgetPreferCol
  FBuffer { undos = uv } <- ask
  reversed <- withImpl (getActionB update)
  lift $ modifyMVar_ uv $ \u -> return $ addUR u reversed
  tell [update]
    

-- | Write an element into the buffer at the current point
-- This is an unsafe operation, no bounds checks are performed
writeB :: Char -> BufferM ()
writeB c = do 
  off <- pointB
  mapM_ applyUpdate [Delete off 1, Insert off [c]]

------------------------------------------------------------------------

-- | Insert the list at current point, extending size of buffer
insertN :: [Char] -> BufferM ()
insertN cs = do 
  pnt <- pointB
  applyUpdate (Insert pnt cs)

------------------------------------------------------------------------

-- | @deleteNAt n p@ deletes @n@ characters forwards from position @p@
deleteNAt :: Int -> Int -> BufferM ()
deleteNAt n pos = applyUpdate (Delete pos n)

------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLn :: BufferM Int
curLn = withImpl curLnI 

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLn :: Int -> BufferM Int
gotoLn = withImpl . gotoLnI

---------------------------------------------------------------------

-- | Return index of next string in buffer that matches argument
searchB :: [Char] -> BufferM (Maybe Int)
searchB = withImpl . searchBI

-- | Set name of syntax highlighting mode
setSyntaxB :: [Char] -> BufferM ()
setSyntaxB = withImpl . setSyntaxBI

-- | Return indices of next string in buffer matched by regex
regexB :: Regex -> BufferM (Maybe (Int,Int))
regexB = withImpl . regexBI

---------------------------------------------------------------------

-- | Set a mark in this buffer
setMarkPointB :: Mark -> Int -> BufferM ()
setMarkPointB m pos = withImpl $ setMarkPointBI m pos

getMarkPointB :: Mark -> BufferM Int
getMarkPointB = withImpl . getMarkPointBI

unsetMarkB :: BufferM ()
unsetMarkB = withImpl unsetMarkBI

getMarkB :: Maybe String -> BufferM Mark
getMarkB = withImpl . getMarkBI

getSelectionMarkB :: BufferM Mark
getSelectionMarkB = withImpl getSelectionMarkBI

-- | Move point -1
leftB :: BufferM ()
leftB = leftN 1

-- | Move cursor -n
leftN :: Int -> BufferM ()
leftN n = pointB >>= \p -> moveTo (p - n)

-- | Move cursor +1
rightB :: BufferM ()
rightB = rightN 1

-- | Move cursor +n
rightN :: Int -> BufferM ()
rightN n = pointB >>= \p -> moveTo (p + n)

-- ---------------------------------------------------------------------
-- Line based movement and friends

readPrefCol :: BufferM (Maybe Int)
readPrefCol = lift . readIORef =<< asks preferCol

setPrefCol :: Maybe Int -> BufferM ()
setPrefCol c = do b <- ask; lift $ writeIORef (preferCol b) c

-- | Move point down by @n@ lines. @n@ can be negative.
lineMoveRel :: Int -> BufferM ()
lineMoveRel n = do
  prefCol <- readPrefCol
  targetCol <- case prefCol of
    Nothing -> offsetFromSol
    Just x -> return x
  gotoLnFrom n
  moveXorEol targetCol
  --logPutStrLn $ "lineMoveRel: targetCol = " ++ show targetCol
  b <- ask; lift $ writeIORef (preferCol b) (Just targetCol)

forgetPreferCol :: BufferM ()
forgetPreferCol = setPrefCol Nothing

savingPrefCol :: BufferM a -> BufferM a
savingPrefCol f = do
  pc <- lift . readIORef =<< asks preferCol
  result <- f
  setPrefCol pc
  return result

-- | Move point up one line
lineUp :: BufferM ()
lineUp = lineMoveRel (-1)

-- | Move point down one line
lineDown :: BufferM ()
lineDown = lineMoveRel 1


-- | Return the contents of the buffer as a list
elemsB :: BufferM [Char]
elemsB = do n <- sizeB
            nelemsB n 0

-- | Read the character at the current point
readB :: BufferM Char
readB = pointB >>= readAtB

-- | Read the character at the given index
-- This is an unsafe operation: character NUL is returned when out of bounds
readAtB :: Int -> BufferM Char
readAtB i = do
    s <- nelemsB 1 i
    return $ case s of
               [c] -> c
               _ -> '\0'

-- | Delete @n@ characters forward from the current point
deleteN :: Int -> BufferM ()
deleteN n = pointB >>= deleteNAt n

-- | Delete to the end of line, excluding it.
deleteToEol :: BufferM ()
deleteToEol = do
    p <- pointB
    moveToEol
    q <- pointB
    deleteNAt (q-p) p

------------------------------------------------------------------------

-- | Return true if the current point is the start of a line
atSol :: BufferM Bool
atSol = do p <- pointB
           if p == 0 then return True
                     else do c <- readAtB (p-1)
                             return (c == '\n')

-- | Return true if the current point is the end of a line
atEol :: BufferM Bool
atEol = do p <- pointB
           e <- sizeB
           if p == e
                  then return True
                  else do c <- readAtB p
                          return (c == '\n')

-- | True if point at start of file
atSof :: BufferM Bool
atSof = do p <- pointB
           return (p == 0)

-- | True if point at end of file
atEof :: BufferM Bool
atEof = do p <- pointB
           e <- sizeB
           return (p == e)


-- | Offset from start of line
offsetFromSol :: BufferM Int
offsetFromSol = savingPrefCol $ do
    i <- pointB
    moveToSol
    j <- pointB
    moveTo i
    return (i - j)
{-# INLINE offsetFromSol #-}

-- | Index of start of line
indexOfSol :: BufferM Int
indexOfSol = savingPrefCol $ do
    i <- pointB
    j <- offsetFromSol
    return (i - j)
{-# INLINE indexOfSol #-}

-- | Index of end of line
indexOfEol :: BufferM Int
indexOfEol = savingPrefCol $ do
    i <- pointB
    moveToEol
    j <- pointB
    moveTo i
    return j
{-# INLINE indexOfEol #-}


-- | Move using the direction specified by the 1st argument, until
-- either we've moved @n@, the 2nd argument, or @p@ the 3rd argument
-- is True
moveAXuntil :: BufferM () -> Int -> (BufferM Bool) -> BufferM ()
moveAXuntil f x p
    | x <= 0    = return ()
    | otherwise = do -- will be slow on long lines...
        let loop 0 = return ()
            loop i = do r <- p
                        when (not r) $ f >> loop (i-1)
        savingPrefCol (loop x)
{-# INLINE moveAXuntil #-}

-- | Move @x@ chars back, or to the sol, whichever is less
moveXorSol :: Int -> BufferM ()
moveXorSol x = moveAXuntil leftB x atSol

-- | Move @x@ chars forward, or to the eol, whichever is less
moveXorEol :: Int -> BufferM ()
moveXorEol x = moveAXuntil rightB x atEol


-- | Go to line indexed from current point
gotoLnFrom :: Int -> BufferM Int
gotoLnFrom x = do 
  l <- curLn
  gotoLn (x+l)

-- | Move point to start of line
moveToSol :: BufferM ()
moveToSol = sizeB >>= moveXorSol  

-- | Move point to end of line
moveToEol :: BufferM ()
moveToEol = sizeB >>= moveXorEol 

getDynamicB :: Initializable a => BufferM a
getDynamicB = do
  b <- ask
  dv <- lift $ readIORef (bufferDynamic b)
  return $ getDynamicValue dv

-- | Insert a value into the extensible state, keyed by its type
setDynamicB :: Initializable a => a -> BufferM ()
setDynamicB x = do
  b <- ask
  lift $ modifyIORef (bufferDynamic b) $ setDynamicValue x
