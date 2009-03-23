{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

-- Copyright (C) 2008 JP Bernardy

module Yi.TextCompletion (
        -- * Word completion
        wordComplete,
        wordCompleteString,
        wordCompleteString',
        resetComplete,
        completeWordB,
) where

import Prelude ()
import Yi.Completion
import Data.Char
import Data.List (map, filter, drop, isPrefixOf, reverse, findIndex, length, groupBy)
import Data.Maybe

import Yi.Core

-- ---------------------------------------------------------------------
-- | Word completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete.


newtype Completion = Completion 
--       Point    -- beginning of the thing we try to complete
      [String] -- the list of all possible things we can complete to.
               -- (this seems very inefficient; but we use lazyness to our advantage)
    deriving Typeable

-- TODO: put this in keymap state instead
instance Initializable Completion where
    initial = Completion []

-- | Switch out of completion mode.
resetComplete :: EditorM ()
resetComplete = setDynamic (Completion [])

-- | Try to complete the current word with occurences found elsewhere in the
-- editor. Further calls try other options. 
wordCompleteString' :: Bool -> EditorM String
wordCompleteString' caseSensitive = do
  Completion complList <- getDynamic
  case complList of
    (x:xs) -> do -- more alternatives, use them.
       setDynamic (Completion xs)
       return x
    [] -> do -- no alternatives, build them.
      w <- withBuffer0 $ do readRegionB =<< regionOfPartB unitWord Backward
      ws <- wordsForCompletion
      setDynamic (Completion $ (nubSet $ filter (matches w) ws) ++ [w])
      -- We put 'w' back at the end so we go back to it after seeing
      -- all possibilities. 
      wordCompleteString -- to pick the 1st possibility.

  -- where matches x y = x `isPrefixOf` y && x /= y
  where matches x y = prepare x `isPrefixOf` prepare y &&
                      x /= y

        prepare x = if caseSensitive then x else map toLower x

wordCompleteString :: EditorM String
wordCompleteString = wordCompleteString' True

wordComplete :: EditorM ()
wordComplete = do x <- wordCompleteString
                  withBuffer0 $ flip replaceRegionB x =<< regionOfPartB unitWord Backward

----------------------------
-- Alternative Word Completion

{-
  'completeWordB' is an alternative to 'wordCompleteB'.

  'completeWordB' offers a slightly different interface. The user
  completes the word using the mini-buffer in the same way a user
  completes a buffer or file name when switching buffers or opening a
  file. This means that it never guesses and completes only as much as
  it can without guessing.

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
     let match :: String -> Maybe String
         match x = if (isPrefixOf curWord x) && (x /= curWord) then Just x else Nothing

     preText             <- completeInList curWord match curWords
     if curWord == ""
        then printMsg "No word to complete"
        else withBuffer0 $ insertN $ drop (length curWord) preText

wordsAndCurrentWord :: BufferM (String, [String])
wordsAndCurrentWord =
  do curText          <- readRegionB =<< regionOfB Document
     curWord          <- readRegionB =<< regionOfPartB unitWord Backward
     return (curWord, words' curText)

wordsForCompletionInBuffer :: BufferM [String]
wordsForCompletionInBuffer = do
  above <- readRegionB =<< regionOfPartB Document Backward
  below <- readRegionB =<< regionOfPartB Document Forward
  return (reverse (words' above) ++ words' below)

wordsForCompletion :: EditorM [String]
wordsForCompletion = do
    (_b:bs) <- fmap bkey <$> getBufferStack
    w0 <- withBuffer0 $ wordsForCompletionInBuffer
    contents <- forM bs $ \b->withGivenBuffer0 b elemsB
    return $ w0 ++ concatMap words' contents

words' :: String -> [String]
words' = filter (not . isNothing . charClass . head) . groupBy ((==) `on` charClass)

charClass :: Char -> Maybe Int
charClass c = findIndex (generalCategory c `elem`)
                [[UppercaseLetter, LowercaseLetter, TitlecaseLetter, ModifierLetter, OtherLetter, 
                  ConnectorPunctuation,
                  NonSpacingMark, SpacingCombiningMark, EnclosingMark, DecimalNumber, LetterNumber, OtherNumber],
                 [MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol]
                ]

{-
  Finally obviously we wish to have a much more sophisticated completeword.
  One which spawns a mini-buffer and allows searching in Hoogle databases
  or in other files etc.
-}

