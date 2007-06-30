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

        -- * Parameterised movement
        doSkipWhile,    -- :: Action -> IO Bool -> (Char -> Bool) -> Action
        doSkipCond,     -- :: Action -> IO Bool -> (Char -> Bool) -> Action
        moveWhileE,     -- :: (Char -> Bool) -> Direction -> Action
        withPointE,     -- :: Action -> Action

        (>>||),         -- :: IO Bool -> IO Bool -> IO Bool

        -- * Word movement
        skipWordE,      -- :: Action
        bskipWordE,     -- :: Action
        firstNonSpaceE, -- :: Action
        nextWordE,      -- :: Action
        prevWordE,      -- :: Action

        -- * Moving to a specific character
        nextCInc,       -- :: Char -> Action
        nextCExc,       -- :: Char -> Action
        prevCInc,       -- :: Char -> Action
        prevCExc,       -- :: Char -> Action

        -- * Paragraph movement
        nextNParagraphs,    -- :: Int -> Action
        prevNParagraphs,    -- :: Int -> Action

        -- * Reading words
        readWordE,      -- :: IO (String,Int,Int)
        readWordLeftE,  -- :: IO (String,Int,Int)

        -- * Word completion
        wordCompleteE,  -- :: Action
        resetCompleteE, -- :: Action

        breadE,         -- :: IO Char

        -- * Delete
        bdeleteE,       -- :: Action
        killWordE,      -- :: Action
        bkillWordE,     -- :: Action

        capitaliseWordE, -- :: Action
        uppercaseWordE,  -- :: Action
        lowercaseWordE,  -- :: Action

        dropSpace,      -- :: String -> String
    ) where

import Yi.Buffer 
import Yi.Core
import Text.Regex.Posix.String ( compExtended, compile, execBlank)
import Yi.Keymap
import Yi.Monad
import Data.Char
import qualified Data.Map as M

import Control.Monad        ( when, replicateM_ )
import Control.Monad.Fix    ( fix )
import Control.Exception    ( assert )
import Control.Monad.Trans

-- For word completion:
import Data.IORef
import System.IO.Unsafe     ( unsafePerformIO )

-- | Read character before point.
breadE :: YiM Char
breadE = do
    p <- withBuffer pointB
    if p == 0
        then return '\0'
        else readNM (p-1) p >>= return . head

--
-- | Perform movement action specified by @mov@ while not @chkend@ and
-- @check@ applied to the 'Char' retuned by @rd@ are true.
--
doSkipWhile :: Action -> YiM Char -> BufferM Bool -> (Char -> Bool) -> Action
doSkipWhile mov rd chkend check = do
    e <- withBuffer chkend
    c <- rd
    when (not e && check c) (mov >> doSkipWhile mov rd chkend check)

--
-- | Similar to 'doSkipWhile', but perform check on the char returned
-- by @rd@, then always move, before branching.
--
doSkipCond :: Action -> YiM Char -> BufferM Bool -> (Char -> Bool) -> Action
doSkipCond mov rd chkend check = do
    c <- rd
    mov
    doSkipWhile mov rd chkend (if check c then check else not . check)

--  Monadic OR operation.
(>>||) :: Monad a => a Bool -> a Bool -> a Bool
a >>|| b = a >>= \ra -> if (not ra) then b else return True

-- Just to make this more easily changed everywhere.
isNonWord :: Char -> Bool
isNonWord = isSpace

-- | Skip to next whitespace or non-whitespace inversely depending on
-- the character under point.
skipWordE :: Action
skipWordE = doSkipCond rightE readE atEol isNonWord

-- | Backwards skip to next whitespace or non-whitespace inversely
-- depending on the character before point.
bskipWordE :: Action
bskipWordE = doSkipCond leftE breadE atSol isNonWord

------------------------------------------------------------------------

-- | Delete one character backward
bdeleteE :: Action
bdeleteE = leftE >> deleteE

-- | Delete forward whitespace or non-whitespace depending on
-- the character under point.
killWordE :: Action
killWordE = doSkipCond deleteE readE atEol isNonWord

-- | Delete backward whitespace or non-whitespace depending on
-- the character before point.
bkillWordE :: Action
bkillWordE = doSkipCond bdeleteE breadE atSol isNonWord

------------------------------------------------------------------------

-- | Move to first char of next word forwards
nextWordE :: Action
nextWordE = do moveWhileE (isAlphaNum) GoRight
               moveWhileE (not.isAlphaNum)  GoRight

-- | Move to first char of next word backwards
prevWordE :: Action
prevWordE = do moveWhileE (isAlphaNum)      GoLeft
               moveWhileE (not.isAlphaNum)  GoLeft

------------------------------------------------------------------------

-- | Move to the next occurence of @c@
nextCInc :: Char -> Action
nextCInc c = rightE >> moveWhileE (/= c) GoRight

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> Action
nextCExc c = nextCInc c >> leftE

-- | Move to the previous occurence of @c@
prevCInc :: Char -> Action
prevCInc c = leftE  >> moveWhileE (/= c) GoLeft

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> Action
prevCExc c = prevCInc c >> rightE

------------------------------------------------------------------------

-- | Move to first non-space character in this line
firstNonSpaceE :: Action
firstNonSpaceE = do
    withBuffer $ do
        moveToSol
        fix $ \loop -> do
            eol <- atEol
            if eol then return ()
                   else do k <- readB
                           when (isSpace k) (rightB >> loop)

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> Action    -- could be rewritten in a more functional style
nextNParagraphs n = do
    withBuffer $ do
        eof <- sizeB
        let loop = do
                p <- pointB
                when (p < eof-1) $ do
                    moveWhile_ (/= '\n') GoRight
                    p' <- pointB
                    when (p' < eof-1) $ do
                        rightB
                        x <- readB
                        when (x /= '\n') loop
        replicateM_ n loop

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> Action
prevNParagraphs n = do
    withBuffer $ do
        let loop = do
                p <- pointB
                when (p > 0) $ do
                    leftB
                    moveWhile_ (/= '\n') GoLeft
                    p' <- pointB
                    when (p' > 0) $ do
                        leftB
                        x <- readB
                        if x == '\n'
                            then rightB
                            else loop
        replicateM_ n loop

------------------------------------------------------------------------
--
-- | Shift the point, until predicate is true, leaving point at final
-- location.

moveWhileE :: (Char -> Bool) -> Direction -> Action
moveWhileE f d = withBuffer (moveWhile_ f d)
--
-- Internal moveWhile function to avoid unnec. ui updates
-- not for external consumption
--
moveWhile_ :: (Char -> Bool)
           -> Direction
           -> BufferM ()

moveWhile_ f dir = do
    eof <- sizeB
    case dir of
        GoRight -> fix $ \loop' -> do p <- pointB
                                      when (p < eof - 1) $ do
                                        x <- readB
                                        when (f x) $ rightB >> loop'
        GoLeft  -> fix $ \loop' -> do p <- pointB
                                      when (p > 0) $ do
                                        x <- readB
                                        when (f x) $ leftB >> loop'

------------------------------------------------------------------------

-- | Read word to the left of the cursor
readWordLeftE :: YiM (String,Int,Int)
readWordLeftE = withBuffer readWordLeft_

-- Core-internal worker
readWordLeft_ :: BufferM (String,Int,Int)
readWordLeft_ = do
    p <- pointB
    c <- readB
    when (not $ isAlphaNum c) $ leftB
    moveWhile_ isAlphaNum GoLeft
    sof <- atSof
    c'  <- readB
    when (not sof || not (isAlphaNum c')) $ rightB
    q <- pointB
    s <- nelemsB (p-q) q
    moveTo p
    return (s,q,p)

-- | Read word under cursor
readWordE :: YiM (String,Int,Int)
readWordE = withBuffer readWord_

------------------------------------------------------------------------

-- | capitalise the word under the cursor
uppercaseWordE :: Action
uppercaseWordE = withPointE $ do
        (_,i,j) <- readWordE
        withBuffer $ moveTo i
        mapRangeE i (j+1) toUpper

-- | lowerise word under the cursor
lowercaseWordE :: Action
lowercaseWordE = withPointE $ do
        (_,i,j) <- readWordE
        withBuffer $ moveTo i
        mapRangeE i (j+1) toLower

-- | capitalise the first letter of this word
capitaliseWordE :: Action
capitaliseWordE = withPointE $ do
        (_,i,_) <- readWordE
        withBuffer $ moveTo i
        mapRangeE i (i+1) toUpper

-- perform an action, and return to the current point
withPointE :: Action -> Action
withPointE f = do p <- withBuffer pointB
                  f
                  withBuffer $ moveTo p

------------------------------------------------------------------------

-- Internal, for readWordE
readWord_ :: BufferM (String,Int,Int)
readWord_ = do
    p <- pointB
    c <- readB
    if not (isAlphaNum c) then leftB
                          else moveWhile_ isAlphaNum GoRight >> leftB
    y <- pointB   -- end point
    moveWhile_ isAlphaNum GoLeft
    sof <- atSof
    c'  <- readB
    when (not sof || not (isAlphaNum c')) $ rightB
    x <- pointB
    s <- nelemsB (y-x+1) x
    moveTo p
    return (s,x,y)

-- ---------------------------------------------------------------------
-- | Word completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete. Finding this word is an IO action.
--

-- remember the word, if any, we're trying to complete, previous matches
-- we've seen, and the point in the search we are up to.
type Completion = (String,M.Map String (),Int)

-- FIXME: This must go in the single editor state. Esp. if we want
-- to do hardcore persistence at some point soon.
--
completions :: IORef (Maybe Completion)
completions = unsafePerformIO $ newIORef Nothing -- FIXME! Unsafeperformio is not welcome in Yi

--
-- | Switch out of completion mode.
--
resetCompleteE :: Action
resetCompleteE = writeRef completions Nothing

--
-- The word-completion action, down the buffer
--
wordCompleteE :: Action
wordCompleteE = withBuffer $ 
        (readRef completions) >>= loop >>= (writeRef completions)

  where
    --
    -- work out where to start our next search
    --
    loop :: (Maybe Completion) -> BufferM (Maybe Completion)
    loop (Just (w,fm,n)) = do
            p  <- pointB
            moveTo (n+1)        -- start where we left off
            doloop p (w,fm)
    loop Nothing = do
            p  <- pointB
            (w,_,_) <- readWordLeft_
            rightB  -- start past point
            doloop p (w,M.singleton w ())

    --
    -- actually do the search, and analyse the result
    --
    doloop :: Int -> (String,M.Map String ())
           -> BufferM (Maybe Completion)

    doloop p (w,fm) = do
            m' <- nextWordMatch w
            moveTo p
            (_,j,_) <- readWord_
            case m' of
                Just (s,i)
                    | j == i                -- seen entire file
                    -> do replaceLeftWith w
                          return Nothing

                    | s `M.member` fm         -- already seen
                    -> loop (Just (w,fm,i))

                    | otherwise             -- new
                    -> do replaceLeftWith s
                          return (Just (w,M.insert s () fm,i))

                Nothing -> loop (Just (w,fm,(-1))) -- goto start of file

    --
    -- replace word under cursor with @s@
    --
    replaceLeftWith :: String -> BufferM ()
    replaceLeftWith s = do
        (_,b,a) <- readWordLeft_     -- back at start
        moveTo b
        deleteN (a-b)
        insertN s

    --
    -- Return next match, and index of that match (to be used for later searches)
    -- Leaves the cursor at the next word.
    --
    nextWordMatch :: String -> BufferM (Maybe (String,Int))
    nextWordMatch w = do
        let re = ("( |\t|\n|\r|^)"++w)
        Right re_c <- liftIO $ compile compExtended execBlank re
        mi   <- regexB re_c
        case mi of
            Nothing -> return Nothing
            Just (i,j) -> do
                c <- readAtB i
                let i' = if i == 0 && isAlphaNum c then 0 else i+1 -- for the space
                moveTo i'
                (s,_,_) <- readWord_
                assert (s /= [] && i /= j) $ return $ Just (s,i')

------------------------------------------------------------------------

-- utility, drop spaces
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f
