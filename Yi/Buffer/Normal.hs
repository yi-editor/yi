-- A normalized (orthogonal) API to many buffer operations
-- This should replace most of the CharMove junk.
module Yi.Buffer.Normal (execB, TextUnit(..), Operation(..), peekB, readUnitB) where

import Yi.Buffer
import Yi.Region
import Yi.Buffer.HighLevel
import Data.Char
import Control.Applicative
import Control.Monad

data TextUnit = Character | Word | Line | Vertical | Paragraph -- | Page | Document | Searched
data Operation = MaybeMove | Move | Delete | Transpose 
               | Transform (String -> String)

isWordChar :: Char -> Bool
isWordChar = isAlpha

isNl :: Char -> Bool
isNl = (== '\n')


-- | Verifies that the list matches all the predicates, pairwise.
checks :: [a -> Bool] -> [a] -> Bool
checks [] _ = True
checks _ [] = False
checks (p:ps) (x:xs) = p x && checks ps xs


-- | read some characters in the specified direction, for boundary testing purposes
peekB :: Direction -> Int -> Int -> BufferM String
peekB dir siz ofs = do
  p <- pointB
  rev dir <$> nelemsB siz (p + dirOfs dir siz ofs)
 
rev :: Direction -> [a] -> [a]
rev Forward = id
rev Backward = reverse

dirOfs :: Direction -> Int -> Int -> Int
dirOfs Forward _siz ofs = ofs
dirOfs Backward siz ofs = 0 - siz - ofs

-- | Is the point at a @Unit@ boundary in the specified @Direction@?
atBoundary :: TextUnit -> Direction -> BufferM Bool
atBoundary Character _ = return True
atBoundary Vertical _ = return True -- a fallacy; this needs a little refactoring.
atBoundary Word direction =
    checks [isWordChar, not . isWordChar] <$> peekB direction 2 (-1)

atBoundary Line direction = checks [isNl] <$> peekB direction 1 0

atBoundary Paragraph direction =
    checks [not . isNl, isNl, isNl] <$> peekB direction 3 (-2)

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

-- TODO: save in the kill ring.
execB Delete unit direction = do
  p <- pointB
  execB Move unit direction
  q <- pointB
  deleteRegionB $ mkRegion p q

execB Transpose unit direction = do
  execB Move unit (opposite direction)
  w0 <- pointB
  execB Move unit direction
  w0' <- pointB
  execB Move unit direction
  w1' <- pointB
  execB Move unit (opposite direction)
  w1 <- pointB
  swapRegions (mkRegion w0 w0') (mkRegion w1 w1')
  moveTo w1'

execB (Transform f) unit direction = do
  p <- pointB
  execB Move unit direction
  q <- pointB
  let r = mkRegion p q
  replaceRegionB r =<< f <$> readRegionB r


regionOfB :: TextUnit -> BufferM Region
regionOfB Line = mkRegion <$> indexOfSol <*> indexOfEol
regionOfB unit = savingPointB $ do
                   execB MaybeMove unit Backward
                   b <- pointB
                   execB Move unit Forward
                   e <- pointB
                   return $ mkRegion b e  

readUnitB :: TextUnit -> BufferM String
readUnitB unit = readRegionB =<< regionOfB unit

opposite :: Direction -> Direction
opposite Backward = Forward
opposite Forward = Backward  

-- | swap the content of two Regions
swapRegions :: Region -> Region -> BufferM ()  
swapRegions r r'
    | regionStart r > regionStart r' = swapRegions r' r
    | otherwise = do w0 <- readRegionB r
                     w1 <- readRegionB r'
                     replaceRegionB r' w0
                     replaceRegionB r  w1
                     

