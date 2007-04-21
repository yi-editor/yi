--
-- Copyright (c) 2007 Jean-Philippe Bernardy
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

-- | A 'Buffer' implementation based on the GTK TextBuffer. Eventually
-- we want to get rid of this, and manage the whole thing ourselves.

module Yi.FastBuffer (Mark, Point, Size, BufferImpl, newBI, deleteNAtI,
                      moveToI, insertNI, pointBI, nelemsBI, finaliseBI, sizeBI, writeBI,
                      curLnI, gotoLnI, searchBI, regexBI, 
                      getMarkPointBI, setMarkPointBI, unsetMarkBI, getMarkBI, getSelectionMarkBI,
                      textbuf, setSyntaxBI, point, updateCursorPosition, fetchCursorPosition)
where

import Prelude hiding (error, mapM)

import Yi.Debug
import Text.Regex.Posix.Wrap

import Data.IORef
import Control.Monad (when)
import Data.Traversable

import Graphics.UI.Gtk hiding ( Point, Size )
import Graphics.UI.Gtk.SourceView

type Point = Int
type Size  = Int
type Mark  = TextMark

data BufferImpl =
        BufferImpl { textbuf :: SourceBuffer
                   , point :: TextMark
                   , mark :: TextMark
                   , markActive :: IORef Bool
                   }

--
-- | read @n@ chars from buffer @b@, starting at @i@
--
readChars :: TextBufferClass b => b -> Point -> Size -> IO String -- ByteString!
readChars buf p i = do 
  start <- textBufferGetIterAtOffset buf (p)
  end <- textBufferGetIterAtOffset buf (p+i)
  result <- textBufferGetText buf start end False
  --logPutStrLn $ "readChars " ++ show p ++ " " ++ show i ++ " = " ++ show result
  return result
{-# INLINE readChars #-}

--
-- | Write string into buffer.
--
writeChars :: TextBufferClass b => b -> [Char] -> Int -> IO ()
writeChars buf cs p = do
  start <- textBufferGetIterAtOffset buf (min 0 p)
  end <- textBufferGetIterAtOffset buf (min 0 (p + length cs))
  textBufferDelete buf start end
  textBufferInsert buf start cs
{-# INLINE writeChars #-}

insertN' :: TextBufferClass b => b -> TextMark -> String -> IO ()
insertN' buf mark cs = do 
  --logPutStrLn "insertN'"
  i <- textBufferGetIterAtMark buf mark
  textBufferInsert buf i cs

deleteN' :: TextBufferClass b => b -> Size -> Point -> IO ()
deleteN' _ 0 _ = return ()
deleteN' tb n p = do
  start <- textBufferGetIterAtOffset tb p
  end <- textBufferGetIterAtOffset tb (p + n)
  textBufferDelete tb start end
{-# INLINE deleteN' #-}


lineMove :: (Int -> Int) -> BufferImpl -> IO ()
lineMove f b = do
  let tb = textbuf b
      p = point b 
  i <- textBufferGetIterAtMark tb p 
  l <- get i textIterLine
  o <- get i textIterLineOffset
  maxL <- textBufferGetLineCount tb
  let newL = f l `inBounds` maxL
  putStrLn $ "lineMove " ++ show (f l) ++ "<" ++ show maxL ++ "  " ++ show l ++ " => " ++ show newL
  set i [textIterLine := newL ]
  print =<< get i textIterLine
  atEnd <- textIterEndsLine i
  when (not atEnd) $ do
       textIterForwardToLineEnd i
       return ()
  print =<< get i textIterLine
  maxO <- get i textIterLineOffset
  set i [textIterLineOffset := min maxO o]
  textBufferMoveMark tb p i

-- | Construct a new buffer initialised with the supplied text
newBI :: [Char] -> IO BufferImpl
newBI s = do
  buf <- sourceBufferNew Nothing
  lm <- sourceLanguagesManagerNew
  Just haskellLang <- sourceLanguagesManagerGetLanguageFromMimeType lm "text/x-haskell"
  sourceBufferSetLanguage buf haskellLang
  sourceBufferSetHighlight buf True
  textBufferSetText buf s
  i <- textBufferGetStartIter buf
  p <- textBufferCreateMark buf (Just "point") i False
  m <- textBufferGetSelectionBound buf
  a <- newIORef False
  let b = BufferImpl buf p m a
  moveToI b 0
  return b

-- | Free any resources associated with this buffer
finaliseBI :: BufferImpl -> IO ()
finaliseBI _ = return () -- gtk takes care of the garbage.

-- | Number of characters in the buffer
sizeBI      :: BufferImpl -> IO Int
sizeBI b = do
  i <- textBufferGetEndIter (textbuf b)
  get i textIterOffset

-- | Extract the current point
pointBI :: BufferImpl -> IO Int
pointBI b = do
  i <- textBufferGetIterAtMark (textbuf b) (point b)
  get i textIterOffset
{-# INLINE pointBI #-}


-- | Return @n@ elems starting at @i@ of the buffer as a list
nelemsBI :: BufferImpl -> Size -> Point -> IO [Char]
nelemsBI b n i = readChars (textbuf b) i n

------------------------------------------------------------------------
-- Point based editing

movePointToIter :: BufferImpl -> TextIter -> IO ()
movePointToIter b p = textBufferMoveMark (textbuf b) (point b) p

-- we maintain our "own" point mark. The upper layers will have to move the insertion 
-- cursor to that point as needed by calling updateCursorPosition
fetchCursorPosition :: BufferImpl -> IO Bool
fetchCursorPosition b = do
  insert <- textBufferGetInsert (textbuf b)
  p <- textBufferGetIterAtMark (textbuf b) (point b)
  q <- textBufferGetIterAtMark (textbuf b) insert  
  o <- get p textIterOffset; logPutStrLn $ "fetchCursorPosition: " ++ show o  
  textBufferMoveMark (textbuf b) (point b) q
  eq <- textIterEqual p q
  return (not eq)

updateCursorPosition :: BufferImpl -> IO ()
updateCursorPosition b = do
  active <- readIORef (markActive b)
  p <- textBufferGetIterAtMark (textbuf b) (point b)
  o <- get p textIterOffset; logPutStrLn $ "updateCursorPosition: " ++ show o
  insert <- textBufferGetInsert (textbuf b) 
  if active
    then textBufferMoveMark (textbuf b) insert p
    else textBufferPlaceCursor (textbuf b) p
  
-- | Move point in buffer to the given index
moveToI :: BufferImpl -> Int -> IO ()
moveToI b off = do
  --logPutStrLn $ "moveTo " ++ show off
  p <- textBufferGetIterAtOffset (textbuf b) off
  movePointToIter b p
{-# INLINE moveToI #-}


-- | Write an element into the buffer at the current point
writeBI :: BufferImpl -> Char -> IO ()
writeBI b c = do
    off <- pointBI b
    deleteN' (textbuf b) 1 off
    writeChars (textbuf b) [c] off 

{-# INLINE writeBI #-}

-- | Insert the list at current point, extending size of buffer
insertNI    :: BufferImpl -> [Char] -> IO ()
insertNI b = insertN' (textbuf b) (point b)

-- | @deleteNAt b n p@ deletes @n@ characters at position @p@
deleteNAtI :: BufferImpl -> Size -> Point -> IO ()
deleteNAtI b = deleteN' (textbuf b)

------------------------------------------------------------------------
-- Line based editing

-- | Return the current line number
curLnI       :: BufferImpl -> IO Int
-- count number of \n from origin to point
curLnI b = do
  p <- textBufferGetIterAtMark (textbuf b) (point b)
  n <- get p textIterLine
  return (n+1)
{-# INLINE curLnI #-}

-- | Go to line number @n@. @n@ is indexed from 1. Returns the
-- actual line we went to (which may be not be the requested line,
-- if it was out of range)
gotoLnI :: BufferImpl -> Int -> IO Int
gotoLnI b n = do
  p <- textBufferGetIterAtMark (textbuf b) (point b)
  textIterSetLine p (max 0 (n-1))
  movePointToIter b p
  textIterGetLine p
{-# INLINE gotoLnI #-}

    ---------------------------------------------------------------------




-- | Return index of next string in buffer that matches argument
searchBI :: BufferImpl -> [Char] -> IO (Maybe Int)
searchBI b s = do
  p <- textBufferGetIterAtMark (textbuf b) (point b)
  mapM (flip get textIterOffset . fst) =<< textIterForwardSearch p s [] Nothing

-- | Return indices of next string in buffer matched by regex
regexBI       :: BufferImpl -> Regex -> IO (Maybe (Int,Int))
regexBI fb re = error "regexBI not implemented"


---------------------------------------------------------------------

getSelectionMarkBI :: BufferImpl -> IO Mark
getSelectionMarkBI b = return (mark b)

getMarkPointBI :: BufferImpl -> Mark -> IO Point
getMarkPointBI b m = do
  i <- textBufferGetIterAtMark (textbuf b) m
  get i textIterOffset


-- | Set this buffer mark (TODO: have a set of these (bookmarks, error list, etc.))
setMarkPointBI :: BufferImpl -> Mark -> Int -> IO ()
setMarkPointBI b m pos = do
  let tb = textbuf b
  logPutStrLn $ "setMarkBI " ++ show pos
  writeIORef (markActive b) True
  p <- textBufferGetIterAtOffset tb pos
  textBufferMoveMark tb m p

{-
  We must allow the unsetting of this mark, this will have the property
  that the point will always be returned as the mark.
-}

unsetMarkBI :: BufferImpl -> IO ()
unsetMarkBI b = do
  let tb = textbuf b
  writeIORef (markActive b) False
  p <- textBufferGetIterAtMark tb (point b)
  textBufferMoveMark tb (mark b) p
  
setSyntaxBI      :: BufferImpl -> String -> IO ()
setSyntaxBI _ _ = return ()
-- | calculate whether a move is in bounds.
inBounds :: Int -> Int -> Int
inBounds i end | i <= 0    = 0
               | i >= end  = max 0 (end - 1)
               | otherwise = i
{-# INLINE inBounds #-}



getMarkBI :: BufferImpl -> Maybe String -> IO Mark
getMarkBI b name = do
  m :: Maybe Mark <- maybe (return Nothing) (textBufferGetMark (textbuf b)) name
  case m of
    Just m' -> return m'
    Nothing -> do
           i <- textBufferGetIterAtMark (textbuf b) (point b)
           textBufferCreateMark (textbuf b) name i False

