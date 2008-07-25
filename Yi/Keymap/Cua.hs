-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua (keymap) where

import Prelude hiding (error)
import Yi.Accessor
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Core
import Yi.Editor
import Yi.File
import Yi.Keymap.Emacs.Utils
import Yi.Keymap.Keys
import Yi.Misc
import Yi.Prelude

keymap :: Keymap
keymap = selfInsertKeymap <|> move <|> select <|> other

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  write (adjBlock 1 >> insertSelf c)

setMark :: BufferM ()
setMark = do
  isSet <- getA highlightSelectionA
  when (not isSet) $ do
       setA highlightSelectionA True
       pointB >>= setSelectionMarkPointB

unsetMark :: BufferM ()
unsetMark = setA highlightSelectionA False

cut, del, copy, paste :: EditorM ()
cut = copy >> del
del = withBuffer0 (deleteRegionB =<< getSelectRegionB)
copy = setRegE =<< withBuffer0 (readRegionB =<< getSelectRegionB)
paste = withBuffer0 . insertN =<< getRegE

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
 spec KBS         ?>>! adjBlock (-1) >> withBuffer bdeleteB,
 spec KDel        ?>>! do
   haveSelection <- withBuffer $ getA highlightSelectionA
   if haveSelection
       then withEditor del
       else adjBlock (-1) >> withBuffer (deleteN 1),
 spec KEnter      ?>>! insertB '\n',
 spec KEsc        ?>>! quitEditor,
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

