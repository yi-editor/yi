--
-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy
--
--

-- This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when
-- convenient. Hence, do not go into the trouble of trying 100%
-- emulation. For example, M-x gives access to Yi (Haskell) functions,
-- with their native names.

module Yi.Keymap.Emacs
  ( keymap
  , makeKeymap
  )
where

import Yi.Yi
import Yi.TextCompletion
import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Utils
  ( KList
  , askQuitEditor
  , completeFileName
  , evalRegionE
  , executeExtendedCommandE
  , findFile
  , insertNextC
  , insertSelf
  , isearchKeymap
  , killBufferE
  , makeKeymap
  , queryReplaceE
  , readArgC
  , scrollDownE
  , scrollUpE
  , shellCommandE
  , cabalConfigureE
  , switchBufferE
  , withMinibuffer
  )
import Yi.Accessor
import Yi.Buffer
import Yi.Buffer.Normal
import Data.Maybe

import Control.Monad
import Control.Applicative

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  Event (KASCII c) [] <- satisfy isPrintableEvent
  write (insertSelf c)
      where isPrintableEvent (Event (KASCII c) []) = c >= ' '
            isPrintableEvent _ = False

keymap :: Keymap
keymap =
  selfInsertKeymap <|> makeKeymap keys

placeMark :: BufferM ()
placeMark = do
  setA highlightSelectionA True
  pointB >>= setSelectionMarkPointB

keys :: KList
keys =
  [ ( "TAB",      write $ withMode $ modeIndent)
  , ( "RET",      write $ repeatingArg $ insertB '\n')
  , ( "DEL",      write $ repeatingArg $ deleteN 1)
  , ( "BACKSP",   write $ repeatingArg bdeleteB)
  , ( "C-M-w",    write $ appendNextKillE)
  , ( "C-/",      write $ repeatingArg undoB)
  , ( "C-_",      write $ repeatingArg undoB)
  , ( "C-<left>", write $ repeatingArg prevWordB)
  , ( "C-<right>",write $ repeatingArg nextWordB)
  , ( "C-<home>", write $ repeatingArg topB)
  , ( "C-<end>",  write $ repeatingArg botB)
  , ( "C-@",      write $ placeMark)
  , ( "C-SPC",    write $ placeMark)
  , ( "C-a",      write $ repeatingArg (maybeMoveB Line Backward))
  , ( "C-b",      write $ repeatingArg leftB)
  , ( "C-d",      write $ repeatingArg $ deleteN 1)
  , ( "C-e",      write $ repeatingArg (maybeMoveB Line Forward))
  , ( "C-f",      write $ repeatingArg rightB)
  , ( "C-g",      write $ setVisibleSelection False)
  -- , ( "C-g",   write $ keyboardQuitE)
  -- C-g should be a more general quit that also unsets the mark.
  , ( "C-i",      write $ withMode $ modeIndent)
  , ( "C-j",      write $ repeatingArg $ insertB '\n')
  , ( "C-k",      write $ killLineE)
  , ( "C-m",      write $ repeatingArg $ insertB '\n')
  , ( "C-n",      write $ repeatingArg $ moveB VLine Forward)
  , ( "C-o",      write $ repeatingArg (insertB '\n' >> leftB))
  , ( "C-p",      write $ repeatingArg $ moveB VLine Backward)
  , ( "C-q",      insertNextC)
  , ( "C-r",      isearchKeymap Backward)
  , ( "C-s",      isearchKeymap Forward)
  , ( "C-t",      write $ repeatingArg $ swapB)
  , ( "C-u",      readArgC)
  , ( "C-v",      write $ scrollDownE)
  , ( "M-v",      write $ scrollUpE)
  , ( "C-w",      write $ killRegionE)
  , ( "C-z",      write $ suspendEditor)
  , ( "C-x C-o",  write deleteBlankLinesB)
  , ( "C-x ^",    write $ repeatingArg enlargeWinE)
  , ( "C-x 0",    write $ closeWindow)
  , ( "C-x 1",    write $ closeOtherE)
  , ( "C-x 2",    write $ splitE)
  , ( "C-x C-c",  write $ askQuitEditor)
  , ( "C-x C-f",  write $ findFile)
  , ( "C-x C-s",  write $ fwriteE)
  , ( "C-x C-w",  write $ withMinibuffer "Write file: "
                                          (completeFileName Nothing)
                                          fwriteToE
    )
  , ( "C-x C-x",  write $ (exchangePointAndMarkB >> setA highlightSelectionA True))
  , ( "C-x b",    write $ switchBufferE)
  , ( "C-x d",    write $ dired)
  , ( "C-x e e",  write $ evalRegionE)
  , ( "C-x o",    write $ nextWinE)
  , ( "C-x k",    write $ killBufferE)
  -- , ( "C-x r k",  write $ killRectE)
  -- , ( "C-x r o",  write $ openRectE)
  -- , ( "C-x r t",  write $ stringRectE)
  -- , ( "C-x r y",  write $ yankRectE)
  , ( "C-x u",    write $ repeatingArg undoB)
  , ( "C-x v",    write $ repeatingArg shrinkWinE)
  , ( "C-y",      write $ yankE)
  , ( "M-!",      write $ shellCommandE)
  , ( "M-p",      write $ cabalConfigureE)
  , ( "M-/",      write $ wordCompleteB)
  , ( "M-<",      write $ repeatingArg topB)
  , ( "M->",      write $ repeatingArg botB)
  , ( "M-%",      write $ queryReplaceE)
  , ( "M-BACKSP", write $ repeatingArg bkillWordB)
  --  , ( "M-a",      write $ repeatingArg backwardSentenceE)
  , ( "M-b",      write $ repeatingArg prevWordB)
  , ( "M-c",      write $ repeatingArg capitaliseWordB)
  , ( "M-d",      write $ repeatingArg killWordB)
  -- , ( "M-e",      write $ repeatingArg forwardSentenceE)
  , ( "M-f",      write $ repeatingArg nextWordB)
  , ( "M-g g",    write $ gotoLn)
  -- , ( "M-h",      write $ repeatingArg markParagraphE)
  -- , ( "M-k",      write $ repeatingArg killSentenceE)
  , ( "M-l",      write $ repeatingArg lowercaseWordB)
  , ( "M-t",      write $ repeatingArg $ transposeB Word Forward)
  , ( "M-u",      write $ repeatingArg uppercaseWordB)
  , ( "M-w",      write $ killRingSaveE)
  , ( "M-x",      write $ executeExtendedCommandE)
  , ( "M-y",      write $ yankPopE)
  , ( "<home>",   write $ repeatingArg moveToSol)
  , ( "<end>",    write $ repeatingArg moveToEol)
  , ( "<left>",   write $ repeatingArg leftB)
  , ( "<right>",  write $ repeatingArg rightB)
  , ( "<up>",     write $ repeatingArg (moveB VLine Backward))
  , ( "<down>",   write $ repeatingArg (moveB VLine Forward))
  , ( "C-<up>",   write $ repeatingArg $ prevNParagraphs 1)
  , ( "C-<down>", write $ repeatingArg $ nextNParagraphs 1)
  , ( "<next>",   write $ repeatingArg downScreenB)
  , ( "<prior>",  write $ repeatingArg upScreenB)
  ]

