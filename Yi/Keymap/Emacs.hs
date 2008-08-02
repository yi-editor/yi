-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

-- | This module aims at a mode that should be (mostly) intuitive to
-- emacs users, but mapping things into the Yi world when
-- convenient. Hence, do not go into the trouble of trying 100%
-- emulation. For example, M-x gives access to Yi (Haskell) functions,
-- with their native names.

module Yi.Keymap.Emacs (keymap)
where
import Control.Applicative
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Core
import Yi.Dired
import Yi.Editor
import Yi.Keymap.Keys
import Yi.File
import Yi.Misc
import Yi.Rectangle
import Yi.TextCompletion
import Yi.Keymap.Keys
import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.UnivArgument
import Yi.Keymap.Emacs.Utils
  ( askQuitEditor
  , adjIndent
  , evalRegionE
  , executeExtendedCommandE
  , findFile
  , insertNextC
  , insertSelf
  , isearchKeymap
  , killBufferE
  , queryReplaceE
  , readArgC
  , scrollDownE
  , scrollUpE
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

keymap :: Keymap
keymap = selfInsertKeymap <|> emacsKeys  <|> completionKm

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  write (adjBlock 1 >> insertSelf c)

completionKm :: Keymap
completionKm = do some (adjustPriority (-1) (meta (char '/') ?>>! wordComplete))
                  write resetComplete
           -- 'adjustPriority' is there to lift the ambiguity between "continuing" completion
           -- and resetting it (restarting at the 1st completion).


placeMark :: BufferM ()
placeMark = do
  setA highlightSelectionA True
  pointB >>= setSelectionMarkPointB

deleteB' :: YiM ()
deleteB' = do
  (adjBlock (-1) >> withBuffer (deleteN 1))

emacsKeys :: Keymap
emacsKeys =
  choice [ -- First all the special key bindings
           spec KTab            ?>>! (adjIndent DecreaseCycle)
         , (shift $ spec KTab)  ?>>! (adjIndent IncreaseCycle)
         , spec KEnter          ?>>! (repeatingArg $ insertB '\n')
         , spec KDel            ?>>! (repeatingArg deleteB')
         , spec KBS             ?>>! (repeatingArg (adjBlock (-1) >> 
                                                   withBuffer bdeleteB))
         , spec KHome           ?>>! (repeatingArg moveToSol)
         , spec KEnd            ?>>! (repeatingArg moveToEol)
         , spec KLeft           ?>>! (repeatingArg leftB)
         , spec KRight          ?>>! (repeatingArg rightB)
         , spec KUp             ?>>! (repeatingArg $ moveB VLine Backward)
         , spec KDown           ?>>! (repeatingArg $ moveB VLine Forward)
         , spec KPageDown       ?>>! (repeatingArg downScreenB)
         , spec KPageUp         ?>>! (repeatingArg upScreenB)

         -- All the keybindings of the form 'Ctrl + special key'
         , (ctrl $ spec KLeft)  ?>>! (repeatingArg prevWordB)
         , (ctrl $ spec KRight) ?>>! (repeatingArg nextWordB)
         , (ctrl $ spec KHome)  ?>>! (repeatingArg topB)
         , (ctrl $ spec KEnd)   ?>>! (repeatingArg botB)
         , (ctrl $ spec KUp)    ?>>! (repeatingArg $ prevNParagraphs 1)
         , (ctrl $ spec KDown)  ?>>! (repeatingArg $ nextNParagraphs 1)

         -- All the keybindings of the form "C-c" where 'c' is some character
         , ctrlCh '@'           ?>>! placeMark
         , ctrlCh ' '           ?>>! placeMark
         , ctrlCh '/'           ?>>! repeatingArg undoB
         , ctrlCh '_'           ?>>! repeatingArg undoB
         , ctrlCh 'a'           ?>>! (repeatingArg (maybeMoveB Line Backward))
         , ctrlCh 'b'           ?>>! (repeatingArg leftB)
         , ctrlCh 'd'           ?>>! (repeatingArg deleteB')
         , ctrlCh 'e'           ?>>! (repeatingArg (maybeMoveB Line Forward))
         , ctrlCh 'f'           ?>>! (repeatingArg rightB)
         , ctrlCh 'g'           ?>>! (setVisibleSelection False)               
         , ctrlCh 'i'           ?>>! (adjIndent IncreaseOnly)
         , ctrlCh 'j'           ?>>! (repeatingArg $ insertB '\n')
         , ctrlCh 'k'           ?>>!  killLineE
         , ctrlCh 'm'           ?>>! (repeatingArg $ insertB '\n')
         , ctrlCh 'n'           ?>>! (repeatingArg $ moveB VLine Forward)
         , ctrlCh 'o'           ?>>! (repeatingArg (insertB '\n' >> leftB))
         , ctrlCh 'p'           ?>>! (repeatingArg $ moveB VLine Backward)
         , ctrlCh 'q'           ?>>  insertNextC
         , ctrlCh 'r'           ?>>  (isearchKeymap Backward)
         , ctrlCh 's'           ?>>  (isearchKeymap Forward)
         , ctrlCh 't'           ?>>! (repeatingArg $ swapB)
         , ctrlCh 'u'           ?>>  readArgC
         , ctrlCh 'v'           ?>>! scrollDownE
         , ctrlCh 'w'           ?>>! killRegion
         , ctrlCh 'y'           ?>>! yankE
         , ctrlCh 'z'           ?>>! suspendEditor

         -- All the keybindings of the form "C-M-c" where 'c' is some character
         , ( ctrl $ metaCh 'w') ?>>! appendNextKillE

         -- All the key-bindings which are preceded by a 'C-x'
         , ctrlCh 'x' ?>>      ctrlX
          
         -- All The key-bindings of the form M-c where 'c' is some character.
         , metaCh 'v'           ?>>! scrollUpE
         , metaCh '!'           ?>>! shellCommandE
         , metaCh 'p'           ?>>! cabalConfigureE
         , metaCh '<'           ?>>! (repeatingArg topB)
         , metaCh '>'           ?>>! (repeatingArg botB)
         , metaCh '%'           ?>>! queryReplaceE
         -- metaCh 'a'          ?>>! (repeatingArg backwardSentenceE)
         , metaCh 'b'           ?>>! (repeatingArg prevWordB)
         , metaCh 'c'           ?>>! (repeatingArg capitaliseWordB)
         , metaCh 'd'           ?>>! (repeatingArg killWordB)
         -- , metaCh 'e'        ?>>! (repeatingArg forwardSentenceE)
         , metaCh 'f'           ?>>! (repeatingArg nextWordB)
         -- , metaCh 'h'        ?>>! (repeatingArg markParagraphE)
         -- , metaCh 'k'        ?>>! (repeatingArg killSentenceE)
         , metaCh 'l'           ?>>! (repeatingArg lowercaseWordB)
         , metaCh 'q'           ?>>! (withSyntax modePrettify)
         , metaCh 'u'           ?>>! (repeatingArg uppercaseWordB)
         , metaCh 't'           ?>>! (repeatingArg $ transposeB Word Forward)
         , metaCh 'w'           ?>>! killRingSaveE
         , metaCh 'x'           ?>>! executeExtendedCommandE
         , metaCh 'y'           ?>>! yankPopE

         -- Other meta key-bindings
         , meta (spec KBS)      ?>>! (repeatingArg bkillWordB)
         , metaCh 'g' ?>> 
             char 'g'           ?>>! gotoLn
         ]
  where
  rectangleFuntions = choice [char 'o' ?>>! openRectangle,
                              char 't' ?>>! stringRectangle,
                              char 'k' ?>>! killRectangle,
                              char 'y' ?>>! yankRectangle
                              ]
  -- These keybindings are all preceded by a 'C-x' so for example to
  -- quit the editor we do a 'C-x C-c'
  ctrlX = choice [ ctrlCh 'o'    ?>>! deleteBlankLinesB
                 , char '^'      ?>>! (repeatingArg enlargeWinE)
                 , char '0'      ?>>! closeWindow
                 , char '1'      ?>>! closeOtherE
                 , char '2'      ?>>! splitE
                 , ctrlCh 'c'    ?>>! askQuitEditor
                 , ctrlCh 'f'    ?>>! findFile
                 , ctrlCh 's'    ?>>! fwriteE
                 , ctrlCh 'w'    ?>>! (withMinibuffer "Write file:"
                                           (matchingFileNames Nothing)
                                           fwriteToE
                                      )
                 , ctrlCh 'x'    ?>>! (exchangePointAndMarkB >> 
                                       setA highlightSelectionA True)
                 , char 'b'      ?>>! switchBufferE
                 , char 'd'      ?>>! dired
                 , char 'e' ?>> 
                   char 'e'      ?>>! evalRegionE
                 , char 'o'      ?>>! nextWinE
                 , char 'k'      ?>>! killBufferE
                 , char 'r'      ?>> rectangleFuntions
                 , char 'u'      ?>>! (repeatingArg undoB)
                 , char 'v'      ?>>! (repeatingArg shrinkWinE)
                 ]
  
