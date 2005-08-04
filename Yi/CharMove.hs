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
import Yi.FastBuffer    (FBuffer)
import Yi.Core
import Yi.Editor
import Yi.Window
import Yi.Regex
import qualified Yi.Map as M

import Data.Char
import Control.Monad        ( when, replicateM_ )
import Control.Monad.Fix    ( fix )
import Control.Exception    ( assert )

-- For word completion:
import Data.IORef
import System.IO.Unsafe     ( unsafePerformIO )

-- i.e. Left or Right
type Direction = () -> Either () ()

-- | Read character before point.
breadE :: IO Char
breadE = do
    p <- getPointE
    if p == 0
        then return '\0'
        else readNM (p-1) p >>= return . head

--
-- | Perform movement action specified by @mov@ while not @chkend@ and
-- @check@ applied to the 'Char' retuned by @rd@ are true.
--
doSkipWhile :: Action -> IO Char -> IO Bool -> (Char -> Bool) -> Action
doSkipWhile mov rd chkend check = do
    e <- chkend
    c <- rd
    when (not e && check c) (mov >> doSkipWhile mov rd chkend check)

--
-- | Similar to 'doSkipWhile', but perform check on the char returned
-- by @rd@, then always move, before branching.
--
doSkipCond :: Action -> IO Char -> IO Bool -> (Char -> Bool) -> Action
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
skipWordE = doSkipCond rightE readE atEolE isNonWord

-- | Backwards skip to next whitespace or non-whitespace inversely 
-- depending on the character before point.
bskipWordE :: Action
bskipWordE = doSkipCond leftE breadE atSolE isNonWord

------------------------------------------------------------------------

-- | Delete one character backward
bdeleteE :: Action
bdeleteE = leftE >> deleteE

-- | Delete forward whitespace or non-whitespace depending on
-- the character under point.
killWordE :: Action
killWordE = doSkipCond deleteE readE atEolE isNonWord

-- | Delete backward whitespace or non-whitespace depending on
-- the character before point.
bkillWordE :: Action
bkillWordE = doSkipCond bdeleteE breadE atSolE isNonWord

------------------------------------------------------------------------

-- | Move to first char of next word forwards
nextWordE :: Action
nextWordE = do moveWhileE (isAlphaNum) Right
               moveWhileE (not.isAlphaNum)  Right

-- | Move to first char of next word backwards
prevWordE :: Action
prevWordE = do moveWhileE (isAlphaNum)      Left
               moveWhileE (not.isAlphaNum)  Left

------------------------------------------------------------------------

-- | Move to the next occurence of @c@
nextCInc :: Char -> Action
nextCInc c = rightE >> moveWhileE (/= c) Right

-- | Move to the character before the next occurence of @c@
nextCExc :: Char -> Action
nextCExc c = nextCInc c >> leftE

-- | Move to thhe previous occurence of @c@
prevCInc :: Char -> Action
prevCInc c = leftE  >> moveWhileE (/= c) Left

-- | Move to the character after the previous occurence of @c@
prevCExc :: Char -> Action
prevCExc c = prevCInc c >> rightE

------------------------------------------------------------------------

-- | Move to first non-space character in this line
firstNonSpaceE :: Action
firstNonSpaceE = do
    withWindow_ $ \w b -> do
        moveToSol b
        fix $ \loop -> do 
            eol <- atEol b
            if eol then return ()
                   else do k <- readB b
                           when (isSpace k) (rightB b >> loop)
        update w b
    getPointE >>= gotoPointE

-- | Move down next @n@ paragraphs
nextNParagraphs :: Int -> Action    -- could be rewritten in a more functional style
nextNParagraphs n = do
    withWindow_ $ \w b -> do
        eof <- sizeB b
        let loop = do
                p <- pointB b
                when (p < eof-1) $ do
                    moveWhile_ (/= '\n') Right w b
                    p' <- pointB b
                    when (p' < eof-1) $ do
                        rightB b
                        x <- readB b
                        when (x /= '\n') loop
        replicateM_ n loop
        return w
    getPointE >>= gotoPointE

-- | Move up prev @n@ paragraphs
prevNParagraphs :: Int -> Action
prevNParagraphs n = do
    withWindow_ $ \w b -> do
        let loop = do
                p <- pointB b
                when (p > 0) $ do
                    leftB b
                    moveWhile_ (/= '\n') Left w b
                    p' <- pointB b
                    when (p' > 0) $ do
                        leftB b
                        x <- readB b
                        if x == '\n'
                            then rightB b
                            else loop
        replicateM_ n loop
        return w
    getPointE >>= gotoPointE

------------------------------------------------------------------------
--
-- | Shift the point, until predicate is true, leaving point at final
-- location. Direction is either False=left, True=right

--
-- N.B. we're using partially applied Left and Right as well-named Bools.
--
-- Maybe this shouldn't refresh?
--
moveWhileE :: (Char -> Bool) -> Direction -> Action
moveWhileE f d = do withWindow_ (moveWhile_ f d)
                    getPointE >>= gotoPointE

--
-- Internal moveWhile function to avoid unnec. ui updates
-- not for external consumption
--
moveWhile_ :: (Char -> Bool)
           -> Direction
           -> Window
           -> FBuffer
           -> IO Window

moveWhile_ f dir w b = do
    eof <- sizeB b
    case dir () of
        Right _ -> fix $ \loop' -> do p <- pointB b
                                      when (p < eof - 1) $ do
                                      x <- readB b
                                      when (f x) $ rightB b >> loop'
        Left  _ -> fix $ \loop' -> do p <- pointB b
                                      when (p > 0) $ do
                                      x <- readB b
                                      when (f x) $ leftB b >> loop'
    return w

------------------------------------------------------------------------

-- | Read word to the left of the cursor
readWordLeftE :: IO (String,Int,Int)
readWordLeftE = withWindow $ \w b -> readWordLeft_ w b >>= \s -> return (w,s)

-- Core-internal worker, not threadsafe.
readWordLeft_ :: Window -> FBuffer -> IO (String,Int,Int)
readWordLeft_ w b = do
    p <- pointB b
    c <- readB b 
    when (not $ isAlphaNum c) $ leftB b
    moveWhile_ isAlphaNum Left w b
    sof <- atSof b
    c'  <- readB b 
    when (not sof || not (isAlphaNum c')) $ rightB b
    q <- pointB b
    s <- nelemsB b (p-q) q
    moveTo b p
    return (s,q,p)

-- | Read word under cursor
readWordE :: IO (String,Int,Int)
readWordE = withWindow $ \w b -> readWord_ w b >>= \v -> return (w,v)

------------------------------------------------------------------------

-- | capitalise the word under the cursor
uppercaseWordE :: Action
uppercaseWordE = withPointE $ do
        (_,i,j) <- readWordE
        gotoPointE i
        mapRangeE i (j+1) toUpper

-- | lowerise word under the cursor
lowercaseWordE :: Action
lowercaseWordE = withPointE $ do
        (_,i,j) <- readWordE
        gotoPointE i
        mapRangeE i (j+1) toLower

-- | capitalise the first letter of this word
capitaliseWordE :: Action
capitaliseWordE = withPointE $ do
        (_,i,_) <- readWordE
        gotoPointE i
        mapRangeE i (i+1) toUpper

-- perform an action, and return to the current point
withPointE :: Action -> Action
withPointE f = do p <- getPointE
                  f
                  gotoPointE p

------------------------------------------------------------------------

-- Internal, for readWordE, not threadsafe
readWord_ :: Window -> FBuffer -> IO (String,Int,Int)
readWord_ w b = do
    p <- pointB b
    c <- readB b 
    if not (isAlphaNum c) then leftB b 
                          else moveWhile_ isAlphaNum Right w b >> leftB b
    y <- pointB b   -- end point
    moveWhile_ isAlphaNum Left w b
    sof <- atSof b
    c'  <- readB b 
    when (not sof || not (isAlphaNum c')) $ rightB b
    x <- pointB b
    s <- nelemsB b (y-x+1) x
    moveTo b p
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

-- This could go in the single editor state, I suppose. Esp. if we want
-- to do hardcore persistence at some point soon.
--
completions :: IORef (Maybe Completion)
completions = unsafePerformIO $ newIORef Nothing

--
-- | Switch out of completion mode.
--
resetCompleteE :: Action
resetCompleteE = writeIORef completions Nothing

--
-- The word-completion action, down the buffer
--
wordCompleteE :: Action
wordCompleteE = do
    withWindow_ $ \win buf -> do
        readIORef completions >>= loop win buf >>= writeIORef completions
        return win
    getPointE >>= gotoPointE

  where
    --
    -- work out where to start our next search
    --
    loop :: Window -> FBuffer -> (Maybe Completion) -> IO (Maybe Completion)
    loop win buf (Just (w,fm,n)) = do
            p  <- pointB buf
            moveTo buf (n+1)        -- start where we left off
            doloop p win buf (w,fm)
    loop win buf Nothing = do 
            p  <- pointB buf
            (w,_,_) <- readWordLeft_ win buf 
            rightB buf  -- start past point
            doloop p win buf (w,M.singleton w ())

    --
    -- actually do the search, and analyse the result
    --
    doloop :: Int -> Window -> FBuffer -> (String,M.Map String ()) 
           -> IO (Maybe Completion)

    doloop p win buf (w,fm) = do
            m' <- nextWordMatch win buf w
            moveTo buf p
            (_,j,_) <- readWord_ win buf
            case m' of
                Just (s,i) 
                    | j == i                -- seen entire file
                    -> do replaceLeftWith win buf w
                          return Nothing

                    | s `M.member` fm         -- already seen
                    -> loop win buf (Just (w,fm,i))

                    | otherwise             -- new
                    -> do replaceLeftWith win buf s
                          return (Just (w,M.insert s () fm,i))

                Nothing -> loop win buf (Just (w,fm,(-1))) -- goto start of file

    --
    -- replace word under cursor with @s@
    --
    replaceLeftWith :: Window -> FBuffer -> String -> IO ()
    replaceLeftWith win buf s = do
        (_,b,a) <- readWordLeft_ win buf     -- back at start
        moveTo buf b
        deleteNW win buf (a-b)
        mapM_ (\c -> insertW c win buf) s

    --
    -- Return next match, and index of that match (to be used for later searches)
    -- Leaves the cursor at the next word.
    --
    nextWordMatch :: Window -> FBuffer -> String -> IO (Maybe (String,Int))
    nextWordMatch win b w = do
        let re = ("( |\t|\n|\r|^)"++w)
        re_c <- regcomp re regExtended
        mi   <- regexB b re_c
        case mi of 
            Nothing -> return Nothing
            Just (i,j) -> do 
                c <- readAtB b i
                let i' = if i == 0 && isAlphaNum c then 0 else i+1 -- for the space
                moveTo b i'
                (s,_,_) <- readWord_ win b
                assert (s /= [] && i /= j) $ return $ Just (s,i')

------------------------------------------------------------------------

-- utility, drop spaces
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f
