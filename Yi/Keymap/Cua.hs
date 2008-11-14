-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua (keymap, portableKeymap) where

import Prelude hiding (error)
import Yi.Core
import Yi.File
import Yi.Keymap.Emacs.Utils
import Yi.Misc

keymap :: Keymap
keymap = portableKeymap ctrl

-- | Introduce a keymap that is compatible with both windows and osx,
--   by parameterising the event modifier required for commands
portableKeymap :: (Event -> Event) -> Keymap
portableKeymap cmd = selfInsertKeymap <|> move <|> select <|> other cmd

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

deleteSel :: BufferM () -> YiM ()
deleteSel act = do
  haveSelection <- withBuffer $ getA highlightSelectionA
  if haveSelection
    then withEditor del
    else withBuffer (adjBlock (-1) >> act)

cut, del, copy, paste :: EditorM ()
cut = copy >> del
del = withBuffer0 (deleteRegionB =<< getSelectRegionB)
copy = setRegE =<< withBuffer0 (readRegionB =<< getSelectRegionB)
paste = withBuffer0 . replaceSel =<< getRegE

moveKeys :: [(Event, BufferM ())]
moveKeys = [
 (spec KHome          , maybeMoveB Line Backward),
 (spec KEnd           , maybeMoveB Line Forward),
 (super (spec KRight) , maybeMoveB Line Forward),
 (super (spec KLeft ) , maybeMoveB Line Backward),
 (ctrl (spec KHome)   , maybeMoveB Document Backward),
 (ctrl (spec KEnd)    , maybeMoveB Document Forward),
 (super (spec KUp)    , maybeMoveB Document Backward),
 (super (spec KDown)  , maybeMoveB Document Forward),
 (ctrl (spec KRight)  , moveB unitWord Forward),
 (ctrl (spec KLeft )  , moveB unitWord Backward),
 (spec KUp            , moveB VLine Backward),
 (spec KDown          , moveB VLine Forward),
 (spec KRight         , moveB Character Forward),
 (spec KLeft          , moveB Character Backward)
 ]


move, select :: Keymap
other :: (Event -> Event) -> Keymap

move   = choice [      k ?>>! unsetMark >> a | (k,a) <- moveKeys]
select = choice [shift k ?>>!   setMark >> a | (k,a) <- moveKeys]
other  cmd = choice [
 spec KBS         ?>>! deleteSel bdeleteB,
 spec KDel        ?>>! deleteSel (deleteN 1),
 spec KEnter      ?>>! replaceSel "\n",
 cmd (char 'q')   ?>>! askQuitEditor,
 cmd (char 'f')   ?>>  isearchKeymap Forward,
 cmd (char 'x')   ?>>! cut,
 cmd (char 'c')   ?>>! copy,
 cmd (char 'v')   ?>>! paste,
 cmd (spec KIns)  ?>>! copy,
 shift (spec KIns) ?>>! paste,
 cmd (char 'z')   ?>>! undoB,
 cmd (char 'y')   ?>>! redoB,
 cmd (char 's')   ?>>! fwriteE,
 cmd (char 'o')   ?>>! findFile,
 cmd (char '/')   ?>>! withModeB modeToggleCommentSelection,
 cmd (char ']')   ?>>! autoIndentB IncreaseOnly,
 cmd (char '[')   ?>>! autoIndentB DecreaseOnly
 ]

