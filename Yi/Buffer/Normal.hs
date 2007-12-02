-- A normalized (orthogonal) API to many buffer operations
-- This should replace most of the CharMove junk.
module Yi.Buffer.Normal (execB, Unit(..), Operation(..)) where

import Yi.Buffer
import Data.Char
import Control.Applicative
import Control.Monad

data Unit = Character | Word | Line | Vertical | Paragraph -- | Page | Document | Searched
data Operation = MaybeMove | Move | Delete | Transpose


indic Forward = 1
indic Backward = negate 1

halfIndic Forward = 0
halfIndic Backward = (-1)

isWordChar = isAlpha

isNl = (== '\n')

-- read some characters in the specified direction, for boundary testing purposes
-- peek :: Direction -> Int -> Int -> BufferM String
-- peek dir siz ofs = do
--   p <- pointB
--   rev dir <$> nelemsB siz (p + dirOfs dir ofs)
-- 
-- rev Forward = id
-- rev Backward = reverse
-- 
-- dirOfs Forward ofs = ofs
-- dirOfs Backward ofs = 0 - 1 - ofs

-- | Is the point at a @Unit@ boundary in the specified @Direction@?
atBoundary :: Unit -> Direction -> BufferM Bool
atBoundary Word direction = do
  p <- pointB
  c <- readAtB (p + halfIndic direction)
  c' <- readAtB (p - 1 - halfIndic direction)
  return (isAlpha c' && not (isAlpha c))

atBoundary Line direction = do
  p <- pointB
  c <- readAtB (p + halfIndic direction)
  return (c == '\n')

atBoundary Paragraph Forward = do
  p <- pointB
  c <- readAtB (p)
  c' <- readAtB (p - 1)
  c'' <- readAtB (p - 2)
  return (c == '\n' && c' == '\n' && c'' /= '\n')


atBoundary Paragraph Backward = do
  p <- pointB
  c <- readAtB (p-1)
  c' <- readAtB (p )
  c'' <- readAtB (p + 1)
  return (c == '\n' && c' == '\n' && c'' /= '\n')



-- | Repeat an action while the condition is fulfilled or the cursor stops moving.
repWhile f cond = do
  stop <- cond
  when (not stop) (repUntil f cond)
  
-- | Repeat an action until the condition is fulfilled or the cursor stops moving.
repUntil f cond = do
  p <- pointB
  f
  p' <- pointB
  stop <- cond
  when (p /= p' && not stop) (repUntil f cond)

-- | Execute the specified triple (operation, unit, direction)
execB :: Operation -> Unit -> Direction -> BufferM ()
execB Move Character Forward = rightB
execB Move Character Backward = leftB
execB Move Vertical Forward = lineDown
execB Move Vertical Backward = lineUp
execB Move unit direction = do
  execB Move Character direction `repUntil` atBoundary unit direction
execB MaybeMove unit direction = do
  execB Move Character direction `repWhile` atBoundary unit direction
  

execB Delete unit direction = do
  p <- pointB
  execB Move unit direction
  q <- pointB
  deleteBetween p q

execB Transpose unit direction = do
  execB Move unit (opposite direction)
  w0 <- pointB
  execB Move unit direction
  w0' <- pointB
  execB Move unit direction
  w1' <- pointB
  execB Move unit (opposite direction)
  w1 <- pointB
  swap (w0,w0') (w1,w1')
  moveTo w1'

opposite Backward = Forward
opposite Forward = Backward  

deleteBetween x y = deleteNAt (abs (x-y)) (min x y) 
readBetween x y = nelemsB (abs (x-y)) (min x y)
replaceBetween x y s = do
  deleteBetween x y
  insertNAt s (min x y)

-- | swap the content of two "regions"
swap :: (Int,Int) -> (Int,Int) -> BufferM ()  
swap (a,b) (x,y) 
    | a > x = swap (x,y) (a,b)
    | otherwise = do w0 <- readBetween a b
                     w1 <- readBetween x y
                     replaceBetween x y w0
                     replaceBetween a b w1
                     

