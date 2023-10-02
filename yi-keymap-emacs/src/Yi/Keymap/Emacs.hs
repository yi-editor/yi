{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Emacs
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when convenient.
-- Hence, do not go into the trouble of trying 100% emulation. For
-- example, @M-x@ gives access to Yi (Haskell) functions, with their
-- native names.

module Yi.Keymap.Emacs ( keymap
                       , mkKeymapSet
                       , defKeymap
                       , ModeMap(..)
                       , eKeymap
                       , completionCaseSensitive
                       ) where

import Control.Applicative      (Alternative ((<|>), empty, some))
import Control.Monad            (replicateM_, unless, void, when)
import Control.Monad.State      (gets)
import Data.Binary              (Binary)
import Data.Char                (digitToInt, isDigit)
import Data.Default             (Default (def))
import Data.Maybe               (fromMaybe)
import Data.Prototype           (Proto (Proto), extractValue)
import Data.Text                ()
import Data.Typeable            (Typeable)
import Lens.Micro.Platform      ((.=), makeLenses, (%=), use)
import Yi.Buffer
import Yi.Buffer.Misc           (updateTransactionInFlightA, getBufferDyn, putBufferDyn)
import Yi.Command               (shellCommandE)
import Yi.Core
import Yi.Dired                 (dired)
import Yi.Editor
import Yi.File                  (fwriteE, fwriteToE)
import Yi.Keymap                (Keymap, KeymapSet, YiAction (..), YiM, modelessKeymapSet, write)
import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.Utils
import Yi.Keymap.Keys
import Yi.MiniBuffer
import Yi.Misc                  (adjIndent, placeMark, selectAll)
import Yi.Mode.Buffers          (listBuffers)
import Yi.Rectangle
import Yi.Search                (isearchFinishWithE, resetRegexE, getRegexE)
import Yi.TextCompletion        (resetComplete, wordComplete')
import Yi.Types                 (YiVariable)

data ModeMap = ModeMap { _eKeymap :: Keymap
                       , _completionCaseSensitive :: Bool
                       }

$(makeLenses ''ModeMap)

-- | Represents how many character have we inserted on a single
--   sequence. Any number greater than 0 means `startUpdateTransactionB`
--   has been run.
newtype ECharCount = ECC Int
  deriving (Binary, Typeable)

instance Default ECharCount where
  def = ECC 0

instance YiVariable ECharCount

keymap :: KeymapSet
keymap = mkKeymapSet defKeymap

mkKeymapSet :: Proto ModeMap -> KeymapSet
mkKeymapSet = modelessKeymapSet . _eKeymap . extractValue

defKeymap :: Proto ModeMap
defKeymap = Proto template
  where
    template self = ModeMap { _eKeymap = emacsKeymap
                            , _completionCaseSensitive = False }
      where
        emacsKeymap :: Keymap
        emacsKeymap = selfInsertKeymap Nothing isDigit <|> completionKm (_completionCaseSensitive self) <|>
             do univArg <- readUniversalArg
                selfInsertKeymap univArg (not . isDigit) <|> emacsKeys univArg

selfInsertKeymap :: Maybe Int -> (Char -> Bool) -> Keymap
selfInsertKeymap univArg condition = do
  c <- printableChar
  unless (condition c) empty
  let n = argToInt univArg
  write $ do
    ECC prevCount <- getBufferDyn @_ @ECharCount
    let newCount0 = prevCount + n
    -- If already on a transacion does nothing
    replicateM_ n (insertB c)
    newCount1 <- if (newCount0 >= 20)
                    then maybeCommitUpdate *> pure 0
                    else pure newCount0
    putBufferDyn (ECC newCount1)

maybeCommitUpdate :: BufferM ()
maybeCommitUpdate = do
  transactionPresent <- use updateTransactionInFlightA
  when transactionPresent $ do
    putBufferDyn (ECC 0)

completionKm :: Bool -> Keymap
completionKm caseSensitive = do
  void $ some (meta (char '/') ?>>! (withCurrentBuffer maybeCommitUpdate
                                     *> wordComplete' caseSensitive))
  deprioritize
  write resetComplete
           -- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
           -- and resetting it (restarting at the 1st completion).

deleteB' :: BufferM ()
deleteB' = deleteN 1

-- | Wrapper around 'moveE' which also cancels incremental search. See
-- issue #499 for details.
moveE :: TextUnit -> Direction -> EditorM ()
moveE u d = do
  getRegexE >>= \case
    -- let's check whether searching is in progress (issues #738, #610)
    Nothing -> return ()
    _ -> isearchFinishWithE resetRegexE
  withCurrentBuffer (moveB u d)

emacsKeys :: Maybe Int -> Keymap
emacsKeys univArg =
  choice [ -- First all the special key bindings
           spec KTab            ?>>! adjIndent IncreaseCycle
         , shift (spec KTab)    ?>>! adjIndent DecreaseCycle
         , spec KEnter          ?>>! repeatingArg newlineB
                                     *> withCurrentBuffer maybeCommitUpdate
         , spec KDel            ?>>! deleteRegionOr deleteForward
         , spec KBS             ?>>! deleteRegionOr deleteBack
         , spec KHome           ?>>! repeatingArg moveToSol
         , spec KEnd            ?>>! repeatingArg moveToEol
         , spec KLeft           ?>>! repeatingArg $ moveE Character Backward
         , spec KRight          ?>>! repeatingArg $ moveE Character Forward
         , spec KUp             ?>>! repeatingArg $ moveE VLine Backward
         , spec KDown           ?>>! repeatingArg $ moveE VLine Forward
         , spec KPageDown       ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg downScreenB
         , spec KPageUp         ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg upScreenB

         , shift (spec KUp)     ?>>! repeatingArg (scrollB (-1))
         , shift (spec KDown)   ?>>! repeatingArg (scrollB 1)

         -- All the keybindings of the form 'Ctrl + special key'
         , ctrl (spec KLeft)    ?>>! repeatingArg prevWordB
         , ctrl (spec KRight)   ?>>! repeatingArg nextWordB
         , ctrl (spec KHome)    ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg topB
         , ctrl (spec KEnd)     ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg botB
         , ctrl (spec KUp)      ?>>! repeatingArg (prevNParagraphs 1)
         , ctrl (spec KDown)    ?>>! repeatingArg (nextNParagraphs 1)

         -- All the keybindings of the form "C-c" where 'c' is some character
         , ctrlCh '@'           ?>>! maybeCommitUpdate *> placeMark
         , ctrlCh ' '           ?>>! maybeCommitUpdate *> placeMark
         , ctrlCh '/'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg undoB
         , ctrlCh '_'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg undoB
         , ctrlCh 'a'           ?>>! repeatingArg (maybeMoveB Line Backward)
         , ctrlCh 'b'           ?>>! repeatingArg $ moveE Character Backward
         , ctrlCh 'd'           ?>>! deleteForward
         , ctrlCh 'e'           ?>>! repeatingArg (maybeMoveB Line Forward)
         , ctrlCh 'f'           ?>>! repeatingArg $ moveE Character Forward
         , ctrlCh 'g'           ?>>! maybeCommitUpdate
                                     *> setVisibleSelection False
         , ctrlCh 'h'           ?>> char 'b' ?>>! acceptedInputsOtherWindow
         , ctrlCh 'i'           ?>>! maybeCommitUpdate
                                     *> adjIndent IncreaseOnly
         , ctrlCh 'j'           ?>>! newlineAndIndentB
         , ctrlCh 'k'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> killLine univArg
         , ctrlCh 'l'           ?>>! (withCurrentBuffer scrollToCursorB >> userForceRefresh)
         , ctrlCh 'm'           ?>>! repeatingArg (insertB '\n')
         , ctrlCh 'n'           ?>>! repeatingArg (moveE VLine Forward)
         , ctrlCh 'o'           ?>>! repeatingArg (insertB '\n' >> leftB)
         , ctrlCh 'p'           ?>>! repeatingArg (moveE VLine Backward)
         , ctrlCh 'q'           ?>>  insertNextC univArg
         , ctrlCh 'r'           ?>>  isearchKeymap Backward
         , ctrlCh 's'           ?>>  isearchKeymap Forward
         , ctrlCh 't'           ?>>! repeatingArg swapB
         , ctrlCh 'v'           ?>>! scrollDownE univArg
         , ctrlCh 'w'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> killRegion
         , ctrlCh 'y'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> yank
         , ctrlCh 'z'           ?>>! suspendEditor
         , ctrlCh '+'           ?>>! repeatingArg (increaseFontSize 1)
         , ctrlCh '-'           ?>>! repeatingArg (decreaseFontSize 1)

         -- All the keybindings of the form "C-M-c" where 'c' is some character
         , ctrl (metaCh 'w')    ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> appendNextKillE
         , ctrl (metaCh ' ')    ?>>! layoutManagersNextE
         , ctrl (metaCh ',')    ?>>! layoutManagerNextVariantE
         , ctrl (metaCh '.')    ?>>! layoutManagerPreviousVariantE
         , ctrl (metaCh 'j')    ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> nextWinE
         , ctrl (metaCh 'k')    ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> prevWinE
         , ctrl (meta $ spec KEnter) ?>>! swapWinWithFirstE


-- All the keybindings of the form "S-C-M-c" where 'c' is some key
         , shift (ctrl $ metaCh 'j') ?>>! moveWinNextE
         , shift (ctrl $ metaCh 'k') ?>>! moveWinPrevE
         , shift (ctrl $ meta $ spec KEnter) ?>>! pushWinToFirstE
         , Event (KASCII ' ') [MShift,MCtrl,MMeta] ?>>! layoutManagersPreviousE

         -- All the key-bindings which are preceded by a 'C-x'
         , ctrlCh 'x' ?>>      ctrlX

         , ctrlCh 'c' ?>>      ctrlC

         -- All The key-bindings of the form M-c where 'c' is some character.
         , metaCh ' '           ?>>! justOneSep univArg
         , metaCh 'v'           ?>>! scrollUpE univArg
         , metaCh '!'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> shellCommandE
         , metaCh '<'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg topB
         , metaCh '>'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg botB
         , metaCh '%'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> queryReplaceE
         , metaCh '^'           ?>>! joinLinesE univArg
         , metaCh ';'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> commentRegion
         , metaCh 'a'           ?>>! repeatingArg (moveE unitSentence Backward)
         , metaCh 'b'           ?>>! repeatingArg prevWordB
         , metaCh 'c'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg capitaliseWordB
         , metaCh 'd'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg killWordB
         , metaCh 'e'           ?>>! repeatingArg (moveE unitSentence Forward)
         , metaCh 'f'           ?>>! repeatingArg nextWordB
         , metaCh 'h'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg (selectNParagraphs 1)
         , metaCh 'k'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg (deleteB unitSentence Forward)
         , metaCh 'l'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg lowercaseWordB
         , metaCh 'm'           ?>>! firstNonSpaceB
         , metaCh 'q'           ?>>! withSyntax modePrettify
         , metaCh 'r'           ?>>! repeatingArg moveToMTB
         , metaCh 'u'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg uppercaseWordB
         , metaCh 't'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg (transposeB unitWord Forward)
         , metaCh 'w'           ?>>! killRingSave
         , metaCh 'x'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> executeExtendedCommandE
         , metaCh 'y'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> yankPopE
         , metaCh '.'           ?>>! promptTag
         , metaCh '{'           ?>>! repeatingArg (prevNParagraphs 1)
         , metaCh '}'           ?>>! repeatingArg (nextNParagraphs 1)
         , metaCh '='           ?>>! countWordsRegion
         , metaCh '\\'          ?>>! maybeCommitUpdate
                                     *> deleteHorizontalSpaceB univArg
         , metaCh '@'           ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg markWord

         -- Other meta key-bindings
         , meta (spec KBS)      ?>>! withCurrentBuffer maybeCommitUpdate
                                     *> repeatingArg bkillWordB
         , metaCh 'g' ?>>
             optMod meta (char 'g') >>! (gotoLn . fromDoc :: Int ::: LineNumber -> BufferM Int)
         ]
  where
  -- inserting the empty string prevents the deletion from appearing in the killring
  -- which is a good thing when we are deleting individuals characters. See
  -- http://code.google.com/p/yi-editor/issues/detail?id=212
  blockKillring = insertN ""

  withUnivArg :: YiAction (m ()) () => (Maybe Int -> m ()) -> YiM ()
  withUnivArg cmd = runAction $ makeAction (cmd univArg)

  repeatingArg :: (Monad m, YiAction (m ()) ()) => m () -> YiM ()
  repeatingArg f = withIntArg $ \n -> replicateM_ n f

  withIntArg :: YiAction (m ()) () => (Int -> m ()) -> YiM ()
  withIntArg cmd = withUnivArg $ \arg -> cmd (fromMaybe 1 arg)

  deleteBack :: YiM ()
  deleteBack = repeatingArg $ blockKillring >> bdeleteB

  deleteForward :: YiM ()
  deleteForward = repeatingArg $ blockKillring >> deleteB'

  -- Deletes current region if any, otherwise executes the given
  -- action.
  deleteRegionOr :: (Show a, YiAction (m a) a) => m a -> YiM ()
  deleteRegionOr f = do
    b <- gets currentBuffer
    r <- withGivenBuffer b getSelectRegionB
    if regionSize r == 0
      then runAction $ makeAction f
      else withGivenBuffer b $ deleteRegionB r

  ctrlC = choice [ ctrlCh 'c' ?>>! withCurrentBuffer maybeCommitUpdate
                                   *> commentRegion ]


  rectangleFunctions = choice
    [ char 'o' ?>>! maybeCommitUpdate
                    *> openRectangle
    , char 't' ?>>! stringRectangle
    , char 'k' ?>>! withCurrentBuffer maybeCommitUpdate
                    *> killRectangle
    , char 'y' ?>>! withCurrentBuffer maybeCommitUpdate
                    *> yankRectangle
         ]

  tabFunctions :: Keymap
  tabFunctions = choice [ optMod ctrl (char 'n') >>! nextTabE
                        , optMod ctrl (char 'p') >>! previousTabE
                        , optMod ctrl (char 't') >>! newTabE
                        , optMod ctrl (char 'e') >>! findFileNewTab
                        , optMod ctrl (char 'd') >>! deleteTabE
                        , charOf id '0' '9' >>=! moveTabE . Just . digitToInt
                        ]
  -- These keybindings are all preceded by a 'C-x' so for example to
  -- quit the editor we do a 'C-x C-c'
  ctrlX = choice [ ctrlCh 'o'    ?>>! maybeCommitUpdate
                                      *> deleteBlankLinesB
                 , char '0'      ?>>! closeWindowEmacs
                 , char '1'      ?>>! closeOtherE
                 , char '2'      ?>>! splitE
                 , char 'h'      ?>>! selectAll
                 , char 's'      ?>>! withCurrentBuffer maybeCommitUpdate
                                      *> askSaveEditor
                 , ctrlCh 'b'    ?>>! listBuffers
                 , ctrlCh 'c'    ?>>! askQuitEditor
                 , ctrlCh 'f'    ?>>! findFile
                 , ctrlCh 'r'    ?>>! findFileReadOnly
                 , ctrlCh 'q'    ?>>!
                     ((withCurrentBuffer (readOnlyA %= not)) :: EditorM ())
                 , ctrlCh 's'    ?>>! fwriteE
                 , ctrlCh 'w'    ?>>! promptFile "Write file:" (void . fwriteToE)
                 , ctrlCh 'x'    ?>>! (exchangePointAndMarkB >>
                                       highlightSelectionA .= True)
                 , char 'b'      ?>>! switchBufferE
                 , char 'd'      ?>>! dired
                 , char 'e' ?>>
                   char 'e'      ?>>! evalRegionE
                 , char 'o'      ?>>! nextWinE
                 , char 'k'      ?>>! killBufferE
                 , char 'r'      ?>>  rectangleFunctions
                 , char 'u'      ?>>! withCurrentBuffer maybeCommitUpdate
                                      *> repeatingArg undoB
                 , optMod ctrl (char 't') >> tabFunctions
                 ]
