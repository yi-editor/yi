{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- Copyright (C) 2008 JP Bernardy

module Yi.TextCompletion (
        -- * Word completion
        wordCompleteB,
        resetCompleteB,
        completeWordB,
) where

import Yi.Completion
import Yi.Buffer
import Data.Char
import Data.Typeable
import Data.List

import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Editor
import Yi.Core

-- ---------------------------------------------------------------------
-- | Word completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete.


newtype Completion = Completion 
      [String] -- the list of all possible things we can complete to.
               -- (this seems very inefficient; but we use lazyness to our advantage)
    deriving Typeable

instance Initializable Completion where
    initial = Completion []

-- | Switch out of completion mode.
resetCompleteB :: BufferM ()
resetCompleteB = setDynamicB (Completion [])

-- The word-completion BufferM (), down the buffer
wordCompleteB :: BufferM ()
wordCompleteB = do
  Completion list <- getDynamicB
  case list of 
    (x:xs) -> do -- more alternatives, use them.
       reg <- regionOfPartB Word Backward       
       replaceRegionB reg x
       setDynamicB (Completion xs)
    [] -> do -- no alternatives, build them.
      w <- readRegionB =<< regionOfPartB Word Backward
      ws <- wordsForCompletion
      setDynamicB (Completion $ (nub $ filter (matches w) ws) ++ [w])
      -- We put 'w' back at the end so we go back to it after seeing
      -- all possibilities. 
      -- NOTE: 'nub' can make searching big lists
      -- quite inefficient. A more clever nub, but still lazy, might
      -- be a good idea.
      wordCompleteB -- to pick the 1st possibility.

  where matches x y = x `isPrefixOf` y && x /= y

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
completeWordB :: EditorM ()
completeWordB = veryQuickCompleteWord


{-
  This is a very quick and dirty way to complete the current word.
  It works in a similar way to the completion of words in the mini-buffer
  it uses the message buffer to give simple feedback such as,
  "Matches:" and "Complete, but not unique:"

  It is by no means perfect but it's also not bad, pretty usable.
-}
veryQuickCompleteWord :: EditorM ()
veryQuickCompleteWord =
  do (curWord, curWords) <- withBuffer0 wordsAndCurrentWord
     let condition :: String -> Bool
         condition x   = (isPrefixOf curWord x) && (x /= curWord)

     preText             <- completeInList curWord condition curWords
     if curWord == ""
        then printMsg "No word to complete"
        else withBuffer0 $ insertN $ drop (length curWord) preText

wordsAndCurrentWord :: BufferM (String, [String])
wordsAndCurrentWord =
  do curText          <- readRegionB =<< regionOfB Document
     curWord          <- readRegionB =<< regionOfPartB Word Backward
     return (curWord, words curText)

wordsForCompletion :: BufferM [String]
wordsForCompletion = do
  above <- readRegionB =<< regionOfPartB Document Backward
  below <- readRegionB =<< regionOfPartB Document Forward
  return (reverse (words above) ++ words below)


{-
  Finally obviously we wish to have a much more sophisticated completeword.
  One which spawns a mini-buffer and allows searching in Hoogle databases
  or in other files etc.
-}

