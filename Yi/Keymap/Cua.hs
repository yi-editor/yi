-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua (keymap) where

import Prelude hiding (error)
import Yi.Core
import Yi.File
import Yi.Keymap.Emacs.Utils
import Yi.Misc

keymap :: Keymap
keymap = selfInsertKeymap <|> move <|> select <|> other

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  write (withBuffer0 $ replaceSel [c])

setMark :: BufferM ()
setMark = do
  isSet <- getA highlightSelectionA
  when (not isSet) $ do
       setA highlightSelectionA True
       pointB >>= setSelectionMarkPointB

unsetMark :: BufferM ()
unsetMark = setA highlightSelectionA False

replaceSel :: String -> BufferM ()
replaceSel s = do
  hasSel <- getA highlightSelectionA
  if hasSel
    then getSelectRegionB >>= flip replaceRegionB s
    else do
      when (length s == 1) (adjBlock 1)
      insertN s

cut, del, copy, paste :: EditorM ()
cut = copy >> del
del = withBuffer0 (deleteRegionB =<< getSelectRegionB)
copy = setRegE =<< withBuffer0 (readRegionB =<< getSelectRegionB)
paste = withBuffer0 . replaceSel =<< getRegE

moveKeys :: [(Event, BufferM ())]
moveKeys = [
 (spec KHome          , maybeMoveB Line Backward),
 (spec KEnd           , maybeMoveB Line Forward),
 (ctrl (spec KHome)   , maybeMoveB Document Backward),
 (ctrl (spec KEnd)    , maybeMoveB Document Forward),
 (ctrl (spec KRight)  , moveB Word Forward),
 (ctrl (spec KLeft )  , moveB Word Backward),
 (spec KUp            , moveB VLine Backward),
 (spec KDown          , moveB VLine Forward),
 (spec KRight         , moveB Character Forward),
 (spec KLeft          , moveB Character Backward)
 ]


move, select, other :: Keymap

move   = choice [      k ?>>! unsetMark >> a | (k,a) <- moveKeys]
select = choice [shift k ?>>!   setMark >> a | (k,a) <- moveKeys]
other = choice [
 spec KBS         ?>>! adjBlock (-1) >> bdeleteB,
 spec KDel        ?>>! do
   haveSelection <- withBuffer $ getA highlightSelectionA
   if haveSelection
       then withEditor del
       else withBuffer (adjBlock (-1) >> deleteN 1),
 spec KEnter      ?>>! insertB '\n',
 ctrl (char 'q')  ?>>! askQuitEditor,
 ctrl (char 'f')  ?>>  isearchKeymap Forward,
 ctrl (char 'x')  ?>>! cut,
 ctrl (char 'c')  ?>>! copy,
 ctrl (char 'v')  ?>>! paste,
 ctrl (spec KIns) ?>>! copy,
 shift (spec KIns) ?>>! paste,
 ctrl (char 'z')  ?>>! undoB,
 ctrl (char 'y')  ?>>! redoB,
 ctrl (char 's')  ?>>! fwriteE,
 ctrl (char 'o')  ?>>! findFile
 ]

