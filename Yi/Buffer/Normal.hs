-- A normalized (orthogonal) API to many buffer operations
-- This should replace most of the CharMove junk.
module Yi.Buffer.Normal (execB, Unit(..), Operation(..)) where

import Yi.Buffer
import Data.Char
-- import Control.Applicative
import Control.Monad

data Unit = Character | Word | Line | Vertical | Paragraph -- | Page | Document | Searched
data Operation = MaybeMove | Move | Delete | Transpose

isWordChar :: Char -> Bool
isWordChar = isAlpha

isNl = (== '\n')


-- | Verifies that the list matches all the predicates, pairwise.
checks :: [a -> Bool] -> [a] -> Bool
checks [] _ = True
checks _ [] = False
checks (p:ps) (x:xs) = p x && checks ps xs


-- | read some characters in the specified direction, for boundary testing purposes
peek :: Direction -> Int -> Int -> BufferM String
peek dir siz ofs = do
  p <- pointB
  rev dir <$> nelemsB siz (p + dirOfs dir siz ofs)
 
rev :: Direction -> [a] -> [a]
rev Forward = id
rev Backward = reverse

dirOfs :: Direction -> Int -> Int -> Int
dirOfs Forward siz ofs = ofs
dirOfs Backward siz ofs = 0 - siz - ofs



-- | Is the point at a @Unit@ boundary in the specified @Direction@?
atBoundary :: TextUnit -> Direction -> BufferM Bool
atBoundary Character _ = return True
atBoundary Word direction =
    checks [isWordChar, not . isWordChar] <$> peek direction 2 (-1)

atBoundary Line direction = checks [isNl] <$> peek direction 1 0

atBoundary Paragraph direction =
    checks [not . isNl, isNl,isNl] <$> peek direction 3 (-2)

-- | Repeat an action while the condition is fulfilled or the cursor stops moving.
repWhile :: BufferM a -> BufferM Bool -> BufferM ()
repWhile f cond = do
  stop <- cond
  when (not stop) (repUntil f cond)
  
-- | Repeat an action until the condition is fulfilled or the cursor stops moving.
repUntil :: BufferM a -> BufferM Bool -> BufferM ()
repUntil f cond = do
  p <- pointB
  f
  p' <- pointB
  stop <- cond
  when (p /= p' && not stop) (repUntil f cond)

-- | Execute the specified triple (operation, unit, direction)
execB :: Operation -> TextUnit -> Direction -> BufferM ()
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

opposite :: Direction -> Direction
opposite Backward = Forward
opposite Forward = Backward  

deleteBetween :: Int -> Int -> BufferM ()
deleteBetween x y = deleteNAt (abs (x-y)) (min x y) 

readBetween :: Int -> Int -> BufferM String
readBetween x y = nelemsB (abs (x-y)) (min x y)

replaceBetween :: Int -> Int -> String -> BufferM ()
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
                     

