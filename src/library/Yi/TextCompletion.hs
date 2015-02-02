{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.TextCompletion
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Module providing text completion functions.

module Yi.TextCompletion (
        -- * Word completion
        wordComplete,
        wordComplete',
        wordCompleteString,
        wordCompleteString',
        mkWordComplete,
        resetComplete,
        completeWordB,
        CompletionScope(..)
) where

import           Control.Applicative ((<$>))
import           Control.Monad       (forM)
import           Data.Binary         (Binary, get, put)
import           Data.Char           (GeneralCategory (..), generalCategory)
import           Data.Default        (Default, def)
import           Data.Function       (on)
import           Data.List           (findIndex)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Maybe          (isJust)
import qualified Data.Text           as T (Text, drop, groupBy, head, isPrefixOf, length, null)
import qualified Data.Text.Encoding  as E (decodeUtf8, encodeUtf8)
import           Data.Typeable       (Typeable)
import           Yi.Buffer
import           Yi.Completion       (completeInList, mkIsPrefixOf)
import           Yi.Editor
import           Yi.Keymap           (YiM)
import qualified Yi.Rope             as R (fromText, toText)
import           Yi.Types            (YiVariable)
import           Yi.Utils            (nubSet)

-- ---------------------------------------------------------------------
-- | Word completion
--
-- when doing keyword completion, we need to keep track of the word
-- we're trying to complete.


newtype Completion = Completion
      [T.Text] -- the list of all possible things we can complete to.
               -- (this seems very inefficient; but we use laziness to
               -- our advantage)
    deriving (Typeable, Show, Eq)

instance Binary Completion where
  put (Completion ts) = put (E.encodeUtf8 <$> ts)
  get = Completion . map E.decodeUtf8 <$> get

-- TODO: put this in keymap state instead
instance Default Completion where
    def = Completion []

instance YiVariable Completion

-- | Switch out of completion mode.
resetComplete :: EditorM ()
resetComplete = putEditorDyn (Completion [])

-- | Try to complete the current word with occurences found elsewhere in the
-- editor. Further calls try other options.
mkWordComplete :: YiM T.Text -- ^ Extract function
               -> (T.Text -> YiM [T.Text]) -- ^ Source function
               -> ([T.Text] -> YiM ()) -- ^ Message function
               -> (T.Text -> T.Text -> Bool) -- ^ Predicate matcher
               -> YiM T.Text
mkWordComplete extractFn sourceFn msgFn predMatch = do
  Completion complList <- withEditor getEditorDyn
  case complList of
    (x:xs) -> do -- more alternatives, use them.
       msgFn (x:xs)
       withEditor . putEditorDyn $ Completion xs
       return x
    [] -> do -- no alternatives, build them.
      w <- extractFn
      ws <- sourceFn w
      let comps = nubSet (filter (matches w) ws) ++ [w]
      putEditorDyn $ Completion comps
      -- We put 'w' back at the end so we go back to it after seeing
      -- all possibilities.

      -- to pick the 1st possibility.
      mkWordComplete extractFn sourceFn msgFn predMatch

  where matches x y = x `predMatch` y && x/=y

wordCompleteString' :: Bool -> YiM T.Text
wordCompleteString' caseSensitive =
  mkWordComplete (withCurrentBuffer $
                   textRegion =<< regionOfPartB unitWord Backward)
                 (\_ -> withEditor wordsForCompletion)
                 (\_ -> return ())
                 (mkIsPrefixOf caseSensitive)
  where
    textRegion = fmap R.toText . readRegionB

wordCompleteString :: YiM T.Text
wordCompleteString = wordCompleteString' True

wordComplete' :: Bool -> YiM ()
wordComplete' caseSensitive = do
  x <- R.fromText <$> wordCompleteString' caseSensitive
  withEditor $ withCurrentBuffer $
    flip replaceRegionB x =<< regionOfPartB unitWord Backward

wordComplete :: YiM ()
wordComplete = wordComplete' True

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
completeWordB :: CompletionScope -> EditorM ()
completeWordB = veryQuickCompleteWord

data CompletionScope = FromCurrentBuffer | FromAllBuffers
  deriving (Eq, Show)

{-
  This is a very quick and dirty way to complete the current word.
  It works in a similar way to the completion of words in the mini-buffer
  it uses the message buffer to give simple feedback such as,
  "Matches:" and "Complete, but not unique:"

  It is by no means perfect but it's also not bad, pretty usable.
-}
veryQuickCompleteWord :: CompletionScope -> EditorM ()
veryQuickCompleteWord scope = do
  (curWord, curWords) <- withCurrentBuffer wordsAndCurrentWord
  allWords <- fmap concat $ withEveryBuffer $ words' <$> (R.toText <$> elemsB)
  let match :: T.Text -> Maybe T.Text
      match x = if (curWord `T.isPrefixOf` x) && (x /= curWord)
                then Just x
                else Nothing
      wordsToChooseFrom = if scope == FromCurrentBuffer
                          then curWords
                          else allWords
  preText             <- completeInList curWord match wordsToChooseFrom
  if T.null curWord
    then printMsg "No word to complete"
    else withCurrentBuffer . insertN . R.fromText $ T.drop (T.length curWord) preText

wordsAndCurrentWord :: BufferM (T.Text, [T.Text])
wordsAndCurrentWord =
  do curText          <- R.toText <$> elemsB
     curWord          <-
       fmap R.toText $ readRegionB =<< regionOfPartB unitWord Backward
     return (curWord, words' curText)

wordsForCompletionInBuffer :: BufferM [T.Text]
wordsForCompletionInBuffer = do
  let readTextRegion = fmap R.toText . readRegionB
  above <- readTextRegion =<< regionOfPartB Document Backward
  below <- readTextRegion =<< regionOfPartB Document Forward
  return $ reverse (words' above) ++ words' below

wordsForCompletion :: EditorM [T.Text]
wordsForCompletion = do
    _ :| bs <- fmap bkey <$> getBufferStack
    w0 <- withCurrentBuffer wordsForCompletionInBuffer
    contents <- forM bs $ \b -> withGivenBuffer b (R.toText <$> elemsB)
    return $ w0 ++ concatMap words' contents

words' :: T.Text -> [T.Text]
words' = filter (isJust . charClass . T.head) . T.groupBy ((==) `on` charClass)

charClass :: Char -> Maybe Int
charClass c = findIndex (generalCategory c `elem`)
                [ [ UppercaseLetter, LowercaseLetter, TitlecaseLetter
                  , ModifierLetter, OtherLetter
                  , ConnectorPunctuation
                  , NonSpacingMark, SpacingCombiningMark, EnclosingMark
                  , DecimalNumber, LetterNumber, OtherNumber
                  ]
                , [ MathSymbol, CurrencySymbol, ModifierSymbol, OtherSymbol ]
                ]

{-
  Finally obviously we wish to have a much more sophisticated completeword.
  One which spawns a mini-buffer and allows searching in Hoogle databases
  or in other files etc.
-}
