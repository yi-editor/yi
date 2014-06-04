{-# LANGUAGE FlexibleContexts, TypeOperators, TemplateHaskell, UnicodeSyntax #-}
-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

-- | This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when
-- convenient. Hence, do not go into the trouble of trying 100%
-- emulation. For example, M-x gives access to Yi (Haskell) functions,
-- with their native names.

module Yi.Keymap.Emacs (keymap,
                        mkKeymap,
                        defKeymap,
                        ModeMap(..),
                        eKeymap,
                        completionCaseSensitive
                        ) where

import Control.Applicative
import Control.Lens
import Data.Prototype
import Yi.Command (shellCommandE)
import Yi.Core
import Yi.Dired
import Yi.File
import Yi.MiniBuffer
import Yi.Misc (adjBlock, adjIndent)
import Yi.Rectangle
import Yi.Search (isearchFinishWithE, resetRegexE)
import Yi.TextCompletion
import Yi.Keymap.Emacs.KillRing
import Yi.Mode.Buffers ( listBuffers )
import Yi.Keymap.Emacs.Utils
  ( askQuitEditor
  , evalRegionE
  , executeExtendedCommandE
  , findFile
  , findFileNewTab
  , promptFile
  , insertNextC
  , isearchKeymap
  , killBufferE
  , queryReplaceE
  , readUniversalArg
  , scrollDownE
  , scrollUpE
  , switchBufferE
  , askSaveEditor
  , argToInt
  , promptTag
  , justOneSep
  , joinLinesE
  )
import Data.Maybe
import Data.Char

import Control.Monad

data ModeMap = ModeMap { _eKeymap :: Keymap
                       , _completionCaseSensitive :: Bool
                       }

$(makeLenses ''ModeMap)

keymap :: KeymapSet
keymap = mkKeymap defKeymap

mkKeymap :: Proto ModeMap -> KeymapSet
mkKeymap = modelessKeymapSet . _eKeymap . extractValue

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
  write (adjBlock n >> replicateM_ n (insertB c))

completionKm :: Bool -> Keymap
completionKm caseSensitive = do void $ some (meta (char '/') ?>>! wordComplete' caseSensitive)
                                deprioritize
                                write resetComplete
           -- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
           -- and resetting it (restarting at the 1st completion).

placeMark :: BufferM ()
placeMark = do
  assign highlightSelectionA True
  pointB >>= setSelectionMarkPointB

selectAll ::BufferM()
selectAll =  botB >> placeMark >> topB >> setVisibleSelection True

deleteB' :: BufferM ()
deleteB' = adjBlock (-1) >> deleteN 1

-- | Wrapper around 'moveE' which also cancels incremental search. See
-- issue #499 for details.
moveE ∷ TextUnit → Direction → EditorM ()
moveE u d = isearchFinishWithE resetRegexE >> withBuffer (moveB u d)

emacsKeys :: Maybe Int -> Keymap
emacsKeys univArg =
  choice [ -- First all the special key bindings
           spec KTab            ?>>! adjIndent IncreaseCycle
         , shift (spec KTab)    ?>>! adjIndent DecreaseCycle
         , spec KEnter          ?>>! repeatingArg (insertB '\n')
         , spec KDel            ?>>! repeatingArg (blockKillring >> deleteB')
         , spec KBS             ?>>! repeatingArg (blockKillring >>
                                                    adjBlock (-1) >>
                                                    bdeleteB)
         , spec KHome           ?>>! repeatingArg moveToSol
         , spec KEnd            ?>>! repeatingArg moveToEol
         , spec KLeft           ?>>! repeatingArg leftB
         , spec KRight          ?>>! repeatingArg rightB
         , spec KUp             ?>>! repeatingArg $ moveE VLine Backward
         , spec KDown           ?>>! repeatingArg $ moveE VLine Forward
         , spec KPageDown       ?>>! repeatingArg downScreenB
         , spec KPageUp         ?>>! repeatingArg upScreenB

         , shift (spec KUp)     ?>>! repeatingArg (scrollB (-1))
         , shift (spec KDown)   ?>>! repeatingArg (scrollB 1)

         -- All the keybindings of the form 'Ctrl + special key'
         , ctrl (spec KLeft)    ?>>! repeatingArg prevWordB
         , ctrl (spec KRight)   ?>>! repeatingArg nextWordB
         , ctrl (spec KHome)    ?>>! repeatingArg topB
         , ctrl (spec KEnd)     ?>>! repeatingArg botB
         , ctrl (spec KUp)      ?>>! repeatingArg (prevNParagraphs 1)
         , ctrl (spec KDown)    ?>>! repeatingArg (nextNParagraphs 1)

         -- All the keybindings of the form "C-c" where 'c' is some character
         , ctrlCh '@'           ?>>! placeMark
         , ctrlCh ' '           ?>>! placeMark
         , ctrlCh '/'           ?>>! repeatingArg undoB
         , ctrlCh '_'           ?>>! repeatingArg undoB
         , ctrlCh 'a'           ?>>! repeatingArg (maybeMoveB Line Backward)
         , ctrlCh 'b'           ?>>! repeatingArg leftB
         , ctrlCh 'd'           ?>>! repeatingArg (blockKillring >> deleteB')
         , ctrlCh 'e'           ?>>! repeatingArg (maybeMoveB Line Forward)
         , ctrlCh 'f'           ?>>! repeatingArg rightB
         , ctrlCh 'g'           ?>>! setVisibleSelection False
         , ctrlCh 'h'           ?>> char 'b' ?>>! acceptedInputsOtherWindow
         , ctrlCh 'i'           ?>>! adjIndent IncreaseOnly
         , ctrlCh 'j'           ?>>! newlineAndIndentB
         , ctrlCh 'k'           ?>>! killLineE univArg
         , ctrlCh 'l'           ?>>! (withBuffer scrollToCursorB >> userForceRefresh)
         , ctrlCh 'm'           ?>>! repeatingArg (insertB '\n')
         , ctrlCh 'n'           ?>>! repeatingArg (moveE VLine Forward)
         , ctrlCh 'o'           ?>>! repeatingArg (insertB '\n' >> leftB)
         , ctrlCh 'p'           ?>>! repeatingArg (moveE VLine Backward)
         , ctrlCh 'q'           ?>>  insertNextC univArg
         , ctrlCh 'r'           ?>>  isearchKeymap Backward
         , ctrlCh 's'           ?>>  isearchKeymap Forward
         , ctrlCh 't'           ?>>! repeatingArg swapB
         , ctrlCh 'v'           ?>>! scrollDownE univArg
         , ctrlCh 'w'           ?>>! killRegion
         , ctrlCh 'y'           ?>>! yankE
         , ctrlCh 'z'           ?>>! suspendEditor

         -- All the keybindings of the form "C-M-c" where 'c' is some character
         , ctrl (metaCh 'w')    ?>>! appendNextKillE
         , ctrl (metaCh ' ')    ?>>! layoutManagersNextE
         , ctrl (metaCh ',')    ?>>! layoutManagerNextVariantE
         , ctrl (metaCh '.')    ?>>! layoutManagerPreviousVariantE
         , ctrl (metaCh 'j')    ?>>! nextWinE
         , ctrl (metaCh 'k')    ?>>! prevWinE
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
         , metaCh ' '           ?>>! justOneSep
         , metaCh 'v'           ?>>! scrollUpE univArg
         , metaCh '!'           ?>>! shellCommandE
         , metaCh '<'           ?>>! repeatingArg topB
         , metaCh '>'           ?>>! repeatingArg botB
         , metaCh '%'           ?>>! queryReplaceE
         , metaCh '^'           ?>>! joinLinesE univArg
         , metaCh ';'           ?>>! withModeY modeToggleCommentSelection
         , metaCh 'a'           ?>>! repeatingArg (moveE unitSentence Backward)
         , metaCh 'b'           ?>>! repeatingArg prevWordB
         , metaCh 'c'           ?>>! repeatingArg capitaliseWordB
         , metaCh 'd'           ?>>! repeatingArg killWordB
         , metaCh 'e'           ?>>! repeatingArg (moveE unitSentence Forward)
         , metaCh 'f'           ?>>! repeatingArg nextWordB
         , metaCh 'h'           ?>>! (setSelectRegionB =<< regionOfB unitParagraph)
         , metaCh 'k'           ?>>! repeatingArg (deleteB unitSentence Forward)
         , metaCh 'l'           ?>>! repeatingArg lowercaseWordB
         , metaCh 'm'           ?>>! firstNonSpaceB
         , metaCh 'q'           ?>>! withSyntax modePrettify
         , metaCh 'u'           ?>>! repeatingArg uppercaseWordB
         , metaCh 't'           ?>>! repeatingArg (transposeB unitWord Forward)
         , metaCh 'w'           ?>>! killRingSaveE
         , metaCh 'x'           ?>>! executeExtendedCommandE
         , metaCh 'y'           ?>>! yankPopE
         , metaCh '.'           ?>>! promptTag
         , metaCh '{'           ?>>! repeatingArg (prevNParagraphs 1)
         , metaCh '}'           ?>>! repeatingArg (nextNParagraphs 1)

         -- Other meta key-bindings
         , meta (spec KBS)      ?>>! repeatingArg bkillWordB
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

  ctrlC = choice [ ctrlCh 'c' ?>>! withModeY modeToggleCommentSelection ]


  rectangleFuntions = choice [char 'a' ?>>! alignRegionOn,
                              char 'o' ?>>! openRectangle,
                              char 't' ?>>! stringRectangle,
                              char 'k' ?>>! killRectangle,
                              char 'y' ?>>! yankRectangle
                              ]

  tabFunctions :: Keymap
  tabFunctions = choice [ optMod ctrl (char 'n') >>! nextTabE
                        , optMod ctrl (char 'p') >>! previousTabE
                        , optMod ctrl (char 't') >>! newTabE
                        , optMod ctrl (char 'e') >>! findFileNewTab
                        , optMod ctrl (char 'd') >>! deleteTabE
                        , charOf id '0' '9' >>=! moveTab . Just . digitToInt
                        ]
  -- These keybindings are all preceded by a 'C-x' so for example to
  -- quit the editor we do a 'C-x C-c'
  ctrlX = choice [ ctrlCh 'o'    ?>>! deleteBlankLinesB
                 , char '^'      ?>>! repeatingArg enlargeWinE
                 , char '0'      ?>>! closeWindow
                 , char '1'      ?>>! closeOtherE
                 , char '2'      ?>>! splitE
                 , char 'h'      ?>>! selectAll
                 , char 's'      ?>>! askSaveEditor
                 , ctrlCh 'b'    ?>>! listBuffers
                 , ctrlCh 'c'    ?>>! askQuitEditor
                 , ctrlCh 'f'    ?>>! findFile
                 , ctrlCh 'q'    ?>>! withBuffer0 ((%=) readOnlyA not)
                 , ctrlCh 's'    ?>>! fwriteE
                 , ctrlCh 'w'    ?>>! promptFile "Write file:" fwriteToE
                 , ctrlCh 'x'    ?>>! (exchangePointAndMarkB >>
                                       assign highlightSelectionA True)
                 , char 'b'      ?>>! switchBufferE
                 , char 'd'      ?>>! dired
                 , char 'e' ?>>
                   char 'e'      ?>>! evalRegionE
                 , char 'o'      ?>>! nextWinE
                 , char 'k'      ?>>! killBufferE
                 , char 'r'      ?>>  rectangleFuntions
                 , char 'u'      ?>>! repeatingArg undoB
                 , char 'v'      ?>>! repeatingArg shrinkWinE
                 , optMod ctrl (char 't') >> tabFunctions
                 ]
