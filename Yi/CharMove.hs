--
-- Copyright (C) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--               2004 Tuomo Valkonen
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
-- | Char-based movement actions.
--
module Yi.CharMove (

        -- * Word movement
        firstNonSpaceB, -- :: BufferM ()
        nextWordB,      -- :: BufferM ()
        prevWordB,      -- :: BufferM ()

        -- * Moving to a specific character
        nextCInc,       -- :: Char -> BufferM ()
        nextCExc,       -- :: Char -> BufferM ()
        prevCInc,       -- :: Char -> BufferM ()
        prevCExc,       -- :: Char -> BufferM ()

        -- * Paragraph movement
        nextNParagraphs,    -- :: Int -> BufferM ()
        prevNParagraphs,    -- :: Int -> BufferM ()

        -- * Reading 
        readLnB,
        readRestOfLnB,

        -- * Word completion
        wordCompleteB,  -- :: BufferM ()
        resetCompleteB, -- :: BufferM ()

        -- * Delete
        bdeleteB,       -- :: BufferM ()
        killWordB,      -- :: BufferM ()
        bkillWordB,     -- :: BufferM ()

        capitaliseWordB, -- :: BufferM ()
        uppercaseWordB,  -- :: BufferM ()
        lowercaseWordB,  -- :: BufferM ()

    ) where

import Yi.Buffer 
import Text.Regex
import Yi.Dynamic
import Data.Char
import Data.Typeable
import qualified Data.Map as M

import Control.Monad        ( when, replicateM_ )
import Control.Applicative
import Control.Monad.Fix    ( fix )
import Control.Exception    ( assert )
import Yi.Buffer.Normal
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.String

------------------------------------------------------------------------

-- | Delete one character backward
bdeleteB :: BufferM ()
bdeleteB = execB Delete Character Backward

-- | Delete forward whitespace or non-whitespace depending on
-- the character under point.
killWordB :: BufferM ()
killWordB = execB Delete Word Forward

-- | Delete backward whitespace or non-whitespace depending on
-- the character before point.
bkillWordB :: BufferM ()
bkillWordB = execB Delete Word Backward

------------------------------------------------------------------------

-- | Move to first char of next word forwards
nextWordB :: BufferM ()
nextWordB = execB Move Word Forward

-- | Move to first char of next word backwards
prevWordB :: BufferM ()
prevWordB = execB Move Word Backward

------------------------------------------------------------------------

-- | Move to the next occurence of @c@
nextCInc :: Char -> BufferM ()
nextCInc c = rightB >> moveWhileB (/= c) Forward

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> BufferM ()
nextCExc c = nextCInc c >> leftB

-- | Move to the previous occurence of @c@
prevCInc :: Char -> BufferM ()
prevCInc c = leftB  >> moveWhileB (/= c) Backward

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> BufferM ()
prevCExc c = prevCInc c >> rightB

------------------------------------------------------------------------

-- | Move to first non-space character in this line
firstNonSpaceB :: BufferM ()
firstNonSpaceB = do
        moveToSol
        fix $ \loop -> do
            eol <- atEol
            if eol then return ()
                   else do k <- readB
                           when (isSpace k) (rightB >> loop)

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> BufferM ()
nextNParagraphs n = replicateM_ n $ execB Move Paragraph Forward

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> BufferM ()
prevNParagraphs n = replicateM_ n $ execB Move Paragraph Backward 

------------------------------------------------------------------------
--
-- | Shift the point, until predicate is true, leaving point at final
-- location.

moveWhileB :: (Char -> Bool) -> Direction -> BufferM ()
moveWhileB f dir = do
    eof <- sizeB
    case dir of
        Forward   -> fix $ \loop' -> do 
                       p <- pointB
                       when (p < eof - 1) $ do
                                       x <- readB
                                       when (f x) $ rightB >> loop'
        Backward -> fix $ \loop' -> do 
                       p <- pointB
                       when (p > 0) $ do
                                       x <- readB
                                       when (f x) $ leftB >> loop'


------------------------------------------------------------------------

-- | capitalise the word under the cursor
uppercaseWordB :: BufferM ()
uppercaseWordB = execB (Transform (map toUpper)) Word Forward

-- | lowerise word under the cursor
lowercaseWordB :: BufferM ()
lowercaseWordB = execB (Transform (map toLower)) Word Forward

-- | capitalise the first letter of this word
capitaliseWordB :: BufferM ()
capitaliseWordB = execB (Transform capitalizeFirst) Word Forward

------------------------------------------------------------------------

-- | Read the line the point is on
readLnB :: BufferM String
readLnB = readUnitB Line

-- | Read from point to end of line
readRestOfLnB :: BufferM String
readRestOfLnB = readRegionB =<< regionOfPartB Line Forward

-- ---------------------------------------------------------------------
-- | Word completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete.
--

-- remember the word, if any, we're trying to complete, previous matches
-- we've seen, and the point in the search we are up to.
newtype Completion = Completion (Maybe (String,M.Map String (),Int)) deriving Typeable

instance Initializable Completion where
    initial = Completion Nothing
--
-- | Switch out of completion mode.
--
resetCompleteB :: BufferM ()
resetCompleteB = setDynamicB (Completion Nothing)

--
-- The word-completion BufferM (), down the buffer
--
wordCompleteB :: BufferM ()
wordCompleteB = getDynamicB >>= loop >>= setDynamicB

  where
    --
    -- work out where to start our next search
    --
    loop :: Completion -> BufferM Completion
    loop (Completion (Just (w,fm,n))) = do
            p  <- pointB
            moveTo (n+1)        -- start where we left off
            doloop p (w,fm)
    loop (Completion Nothing) = do
            p  <- pointB
            w <- readRegionB =<< regionOfPartB Word Backward
            rightB  -- start past point
            doloop p (w,M.singleton w ())

    --
    -- actually do the search, and analyse the result
    --
    doloop :: Int -> (String,M.Map String ())
           -> BufferM Completion

    doloop p (w,fm) = do
            m' <- nextWordMatch w
            moveTo p
            j <-regionStart <$> regionOfB Word
            case m' of
                Just (s,i)
                    | j == i                -- seen entire file
                    -> do replaceLeftWith w
                          return (Completion Nothing)

                    | s `M.member` fm         -- already seen
                    -> loop (Completion (Just (w,fm,i)))

                    | otherwise             -- new
                    -> do replaceLeftWith s
                          return (Completion (Just (w,M.insert s () fm,i)))

                Nothing -> loop (Completion (Just (w,fm,(-1)))) -- goto start of file

    --
    -- replace word under cursor with @s@
    --
    replaceLeftWith :: String -> BufferM ()
    replaceLeftWith s = do 
        r <- regionOfPartB Word Backward     -- back at start
        replaceRegionB r s
        moveTo (regionStart r + length s)

    --
    -- Return next match, and index of that match (to be used for later searches)
    -- Leaves the cursor at the next word.
    --
    nextWordMatch :: String -> BufferM (Maybe (String,Int))
    nextWordMatch w = do
        let re = ("( |\t|\n|\r|^)"++w)
        let re_c = mkRegex re
        mi   <- regexB re_c
        case mi of
            Nothing -> return Nothing
            Just (i,j) -> do
                c <- readAtB i
                let i' = if i == 0 && isAlphaNum c then 0 else i+1 -- for the space
                moveTo i'
                s <- readUnitB Word
                assert (s /= [] && i /= j) $ return $ Just (s,i')

