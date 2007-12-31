--
-- Copyright (c) 2005,2007 Jean-Philippe Bernardy
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

-- This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when
-- convenient. Hence, do not go into the trouble of trying 100%
-- emulation. For example, M-x gives access to Yi (haskell) functions,
-- with their native names.

module Yi.Keymap.Emacs 
  ( keymap
  , makeProcess
  ) 
where

import Yi.Yi

import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Utils
  ( atomic
  , withMinibuffer
  , scrollDownE
  , readArgC
  , isearchProcess
  , Process
  , KList
  , makeProcess

  , insertSelf
  , insertNextC
  , findFile
  , completeFileName
  , switchBufferE
  , evalRegionE
  , gotoLineE
  , killBufferE
  , shellCommandE
  , queryReplaceE
  , executeExtendedCommandE
  )
import Yi.Buffer
import Yi.Buffer.Normal
import Data.Maybe

import Control.Monad
import Control.Applicative

import Yi.Indent

selfInsertKeymap :: Process
selfInsertKeymap = do
  Event (KASCII c) [] <- satisfy isPrintableEvent
  write (insertSelf c)
      where isPrintableEvent (Event (KASCII c) []) = c >= ' '
            isPrintableEvent _ = False

keymap :: Process
keymap = 
  selfInsertKeymap <|> makeProcess keys

keys :: KList
keys =
  [ ( "TAB",      atomic $ autoIndentB)
  , ( "RET",      atomic $ repeatingArg $ insertB '\n')
  , ( "DEL",      atomic $ repeatingArg deleteB)
  , ( "BACKSP",   atomic $ repeatingArg bdeleteB)
  , ( "C-M-w",    atomic $ appendNextKillE)
  , ( "C-/",      atomic $ repeatingArg undoB)
  , ( "C-_",      atomic $ repeatingArg undoB)
  , ( "C-<left>", atomic $ repeatingArg prevWordB)
  , ( "C-<right>",atomic $ repeatingArg nextWordB)
  , ( "C-@",      atomic $ (pointB >>= setSelectionMarkPointB))
  , ( "C-SPC",    atomic $ (pointB >>= setSelectionMarkPointB))
  , ( "C-a",      atomic $ repeatingArg (execB MaybeMove Line Backward))
  , ( "C-b",      atomic $ repeatingArg leftB)
  , ( "C-d",      atomic $ repeatingArg deleteB)
  , ( "C-e",      atomic $ repeatingArg (execB MaybeMove Line Forward))
  , ( "C-f",      atomic $ repeatingArg rightB)
  , ( "C-g",      atomic $ unsetMarkB) 
  -- , ( "C-g",      atomic $ keyboardQuitE)
  -- C-g should be a more general quit that also unsets the mark.
  , ( "C-i",      atomic $ autoIndentB)
  , ( "C-j",      atomic $ repeatingArg $ insertB '\n')
  , ( "C-k",      atomic $ killLineE)
  , ( "C-m",      atomic $ repeatingArg $ insertB '\n')
  , ( "C-n",      atomic $ repeatingArg lineDown)
  , ( "C-o",      atomic $ repeatingArg (insertB '\n' >> leftB))
  , ( "C-p",      atomic $ repeatingArg lineUp)
  , ( "C-q",               insertNextC)
  -- , ( "C-r",      atomic $ backwardsIncrementalSearchE)
  , ( "C-s",      isearchProcess)
  , ( "C-t",      atomic $ repeatingArg $ swapB)
  , ( "C-u",               readArgC)
  , ( "C-v",      atomic $ scrollDownE)
  , ( "C-w",      atomic $ killRegionE)
  , ( "C-z",      atomic $ suspendE)
  , ( "C-x ^",    atomic $ repeatingArg enlargeWinE)
  , ( "C-x 0",    atomic $ closeE)
  , ( "C-x 1",    atomic $ closeOtherE)
  , ( "C-x 2",    atomic $ splitE)
  , ( "C-x C-c",  atomic $ quitE)
  , ( "C-x C-f",  atomic $ findFile)
  , ( "C-x C-s",  atomic $ fwriteE)
  , ( "C-x C-w",  atomic $ withMinibuffer "Write file: " 
                                          (completeFileName Nothing) 
                                          fwriteToE
    )
  , ( "C-x C-x",  atomic $ exchangePointAndMarkB)
  , ( "C-x b",    atomic $ switchBufferE)
  , ( "C-x d",    atomic $ loadE "Yi.Dired" >> execE "Yi.Dired.diredE")
  , ( "C-x e e",  atomic $ evalRegionE)
  , ( "C-x o",    atomic $ nextWinE)
  , ( "C-x l",    atomic $ gotoLineE)
  , ( "C-x k",    atomic $ killBufferE)
  -- , ( "C-x r k",  atomic $ killRectE)
  -- , ( "C-x r o",  atomic $ openRectE)
  -- , ( "C-x r t",  atomic $ stringRectE)
  -- , ( "C-x r y",  atomic $ yankRectE)
  , ( "C-x u",    atomic $ repeatingArg undoB)
  , ( "C-x v",    atomic $ repeatingArg shrinkWinE)
  , ( "C-y",      atomic $ yankE)
  , ( "M-!",      atomic $ shellCommandE)
  , ( "M-/",      atomic $ wordCompleteB)
  , ( "M-<",      atomic $ repeatingArg topB)
  , ( "M->",      atomic $ repeatingArg botB)
  , ( "M-%",      atomic $ queryReplaceE)
  , ( "M-BACKSP", atomic $ repeatingArg bkillWordB)
  --  , ( "M-a",      atomic $ repeatingArg backwardSentenceE)
  , ( "M-b",      atomic $ repeatingArg prevWordB)
  , ( "M-c",      atomic $ repeatingArg capitaliseWordB)
  , ( "M-d",      atomic $ repeatingArg killWordB)
  -- , ( "M-e",      atomic $ repeatingArg forwardSentenceE)
  , ( "M-f",      atomic $ repeatingArg nextWordB)
  -- , ( "M-h",      atomic $ repeatingArg markParagraphE)
  -- , ( "M-k",      atomic $ repeatingArg killSentenceE)
  , ( "M-l",      atomic $ repeatingArg lowercaseWordB)
  -- , ( "M-t",      atomic $ repeatingArg transposeWordsE)
  , ( "M-u",      atomic $ repeatingArg uppercaseWordB)
  , ( "M-w",      atomic $ killRingSaveE)
  , ( "M-x",      atomic $ executeExtendedCommandE)
  , ( "M-y",      atomic $ yankPopE)
  , ( "<home>",   atomic $ repeatingArg moveToSol)
  , ( "<end>",    atomic $ repeatingArg moveToEol)
  , ( "<left>",   atomic $ repeatingArg leftB)
  , ( "<right>",  atomic $ repeatingArg rightB)
  , ( "<up>",     atomic $ repeatingArg (execB Move VLine Backward))
  , ( "<down>",   atomic $ repeatingArg (execB Move VLine Forward))
  , ( "<next>",   atomic $ repeatingArg downScreenE)
  , ( "<prior>",  atomic $ repeatingArg upScreenE)
  ]

