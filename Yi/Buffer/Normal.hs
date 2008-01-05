-- A normalized (orthogonal) API to many buffer operations

module Yi.Buffer.Normal (execB, TextUnit(..), Operation(..), 
                         peekB, regionOfB, regionOfPartB, readUnitB,
                         moveEndB, moveBeginB) where

import Yi.Buffer
import Yi.Buffer.Region
import Data.Char
import Control.Applicative
import Control.Monad

-- | Designate a given "unit" of text.
data TextUnit = Character | Word 
              | ViWord -- ^ a word as in use in Vim
              | Line  -- ^ a line of text (between newlines)
              | VLine -- ^ a "vertical" line of text (area of text between to characters at the same column number)
              | Paragraph 
   -- | Page | Document | Searched

data Operation = Move       -- ^ move the next unit boundary
               | MaybeMove  -- ^ as the above, unless the point is at a unit boundary
               | Delete     -- ^ delete between point and next unit boundary
               | Transpose 
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
peekB dir siz ofs = 
  do p <- pointB
     rev dir <$> nelemsB siz (p + dirOfs)
  where 
  dirOfs = case dir of
             Forward  -> ofs
             Backward -> 0 - siz - ofs

checkPeekB :: Int -> [Char -> Bool] -> Direction -> BufferM Bool
checkPeekB offset conds dir = checks conds <$> peekB dir (length conds) offset

-- | reverse if Backward
rev :: Direction -> [a] -> [a]
rev Forward = id
rev Backward = reverse


-- | Is the point at a @Unit@ boundary in the specified @Direction@?
atBoundary :: TextUnit -> Direction -> BufferM Bool
atBoundary Character _ = return True
atBoundary VLine _ = return True -- a fallacy; this needs a little refactoring.
atBoundary Word direction =
    checkPeekB (-1) [isWordChar, not . isWordChar] direction
atBoundary ViWord direction = do
    ~cs@[c1,c2] <- peekB direction 2 (-1)
    return (length cs /= 2 || (not (isSpace c1) && (charType c1 /= charType c2)))
        where charType c | isSpace c = 1::Int
                         | isAlpha c = 2
                         | otherwise = 3


atBoundary Line direction = checkPeekB 0 [isNl] direction

atBoundary Paragraph direction =
    checkPeekB (-2) [not . isNl, isNl, isNl] direction

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

moveEndB :: TextUnit -> BufferM ()
moveEndB unit = do
  execB Move Character Forward
  execB Move unit Forward
  execB Move Character Backward

moveBeginB :: TextUnit -> Direction -> BufferM ()
moveBeginB unit dir = do
  execB Move Character dir `repUntil` atBoundary unit (opposite dir)


-- | Execute the specified triple (operation, unit, direction)
execB :: Operation -> TextUnit -> Direction -> BufferM ()
execB Move Character Forward  = rightB
execB Move Character Backward = leftB
execB Move VLine Forward      = -- FIXME: this should be O(buffersize)
  do i    <- curLn 
     size <- numberOfLines
     if i == size
       then execB MaybeMove Line Forward
       else lineDown
execB Move VLine Backward     = -- FIXME: this should not be O(buffersize)
  do i <- curLn
     if i == 1
        then execB MaybeMove Line Backward
        else lineUp
execB Move unit direction = do
  execB Move Character direction `repUntil` atBoundary unit direction

-- So for example here moveToEol = execB MaybeMove Line Forward;
-- in that it will move to the end of current line and nowhere if we
-- are already at the end of the current line. Similarly for moveToSol.
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


indexAfterB :: BufferM a -> BufferM Point
indexAfterB f = savingPointB (f >> pointB)

-- | Region of the whole textunit where the current point is
regionOfB :: TextUnit -> BufferM Region
regionOfB unit = mkRegion
                 <$> indexAfterB (execB MaybeMove unit Backward)
                 <*> indexAfterB (execB MaybeMove unit Forward)

-- | Region between the point and the next boundary
regionOfPartB :: TextUnit -> Direction -> BufferM Region
regionOfPartB unit dir = savingPointB $ do
         b <- pointB
         execB MaybeMove unit dir
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
                     

