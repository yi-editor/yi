-- Copyright (C) 2008 JP Bernardy


module Yi.TextCompletion (
        -- * Word completion
        wordCompleteB, 
        resetCompleteB,
        completeWordB,
) where

import Yi.Completion
import Yi.Buffer 
import Text.Regex
import Yi.Dynamic
import Data.Char
import Data.Typeable
import Data.List
import qualified Data.Map as M

import Control.Monad        ( when, replicateM_ )
import Control.Applicative
import Control.Monad.Fix    ( fix )
import Control.Exception    ( assert )
import Yi.Buffer.Normal
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.String
import Yi.Core

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


----------------------------
-- Alternative Word Completion

{-
  'completeWordB' is an alternative to 'wordCompleteB'.
  Currently the main reason for this extra function is that the
  aforementioned is rather buggy, two problems I currently have with
  it is that it occasionally remembers the previous word it was completing
  before and completes that rather than the current one. More seriously
  it occasionally crashes yi by going into an infinite loop.

  In the longer term assuming that 'Yi.CharMove.wordCompleteB' is fixed
  (which I would love) then 'completeWordB' offers a slightly different
  interface. The user completes the word using the mini-buffer in the
  same way a user completes a buffer or file name when switching buffers
  or opening a file. This means that it never guesses and completes
  only as much as it can without guessing.

  I think there is room for both approaches. The 'wordCompleteB' approach
  which just guesses the completion from a list of possible completion
  and then re-hitting the key-binding will cause it to guess again.
  I think this is very nice for things such as completing a word within
  a TeX-buffer. However using the mini-buffer might be nicer when we allow
  syntax knowledge to allow completion for example we may complete from
  a Hoogle database.
-}
completeWordB :: YiM ()
completeWordB = veryQuickCompleteWord


{-
  This is a very quick and dirty way to complete the current word.
  It works in a similar way to the completion of words in the mini-buffer
  it uses the message buffer to give simple feedback such as,
  "Matches:" and "Complete, but not unique:"

  It is by no means perfect but it's also not bad, pretty usable.
-}
veryQuickCompleteWord :: YiM ()
veryQuickCompleteWord =
  do (curWord, curWords) <- withBuffer wordsAndCurrentWord
     let condition :: String -> Bool
         condition x   = (isPrefixOf curWord x) && (x /= curWord)

     preText             <- completeInList curWord condition curWords
     if curWord == ""
        then msgE "No word to complete"
        else withBuffer $ insertN $ drop (length curWord) preText

wordsAndCurrentWord :: BufferM (String, [ String ])
wordsAndCurrentWord =
  do curSize          <- sizeB
     curText          <- readRegionB $ mkRegion 0 curSize
     curWord          <- readRegionB =<< regionOfPartB Word Backward
     let curWords     = words curText
     return (curWord, curWords)

{-
  Finally obviously we wish to have a much more sophisticated completeword.
  One which spawns a mini-buffer and allows searching in Hoogle databases
  or in other files etc.
-}

