-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua (keymap, portableKeymap, cut, paste, copy, del) where

import Prelude (length, take, drop)
import Yi.Core
import Yi.File
import Yi.Keymap.Emacs.Utils
import Yi.Misc (adjBlock)
import Yi.Rectangle
import Yi.String

keymap :: KeymapSet
keymap = portableKeymap ctrl

-- | Introduce a keymap that is compatible with both windows and osx,
--   by parameterising the event modifier required for commands
portableKeymap :: (Event -> Event) -> KeymapSet
portableKeymap cmd = modelessKeymapSet $ selfInsertKeymap <|> move <|> select <|> rect <|> other cmd

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  write (withBuffer0 $ replaceSel [c])

setMark :: Bool -> BufferM ()
setMark b = do
  isSet <- use highlightSelectionA
  rectangleSelectionA .= b
  when (not isSet) $ do
       highlightSelectionA .= True
       pointB >>= setSelectionMarkPointB

unsetMark :: BufferM ()
unsetMark = highlightSelectionA .= False

replaceSel :: String -> BufferM ()
replaceSel s = do
  hasSel <- use highlightSelectionA
  if hasSel
    then getSelectRegionB >>= flip replaceRegionB s
    else do
      when (length s == 1) (adjBlock 1)
      insertN s

deleteSel :: BufferM () -> YiM ()
deleteSel act = do
  haveSelection <- withBuffer $ use highlightSelectionA
  if haveSelection
    then withEditor del
    else withBuffer (adjBlock (-1) >> act)

cut, del, copy, paste :: EditorM ()
cut = copy >> del
del = do
  asRect <- withBuffer0 $ use rectangleSelectionA
  if asRect
    then killRectangle
    else withBuffer0 $ deleteRegionB =<< getSelectRegionB
copy = do
  (setRegE =<<) $ withBuffer0 $ do
    asRect <- use rectangleSelectionA
    if not asRect
      then readRegionB =<< getSelectRegionB
      else do
        (reg, l, r) <- getRectangle
        unlines' <$> fmap (take (r-l) . drop l) <$> lines' <$> readRegionB reg
paste = do
  asRect <- withBuffer0 (use rectangleSelectionA)
  if asRect
    then yankRectangle
    else withBuffer0 . replaceSel =<< getRegE

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


move, select, rect :: Keymap
other :: (Event -> Event) -> Keymap

move   = choice [            k  ?>>! unsetMark       >> a | (k,a) <- moveKeys]
select = choice [      shift k  ?>>!   setMark False >> a | (k,a) <- moveKeys]
rect   = choice [meta (shift k) ?>>!   setMark True  >> a | (k,a) <- moveKeys]
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

