{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Cua
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Cua keymap.

module Yi.Keymap.Cua ( keymap
                     , portableKeymap
                     , customizedCuaKeymapSet
                     , cut
                     , paste
                     , copy
                     , del
                     ) where

import           Control.Applicative      (Alternative ((<|>)))
import           Lens.Micro.Platform      ((.=), use)
import           Control.Monad            (unless, when)
import qualified Data.Text                as T (drop, take)
import           Yi.Buffer
import           Yi.Editor
import           Yi.File                  (fwriteE)
import           Yi.Keymap                (Keymap, KeymapSet, YiM, modelessKeymapSet, write)
import           Yi.Keymap.Emacs.Utils    (askQuitEditor, findFile, isearchKeymap)
import           Yi.Keymap.Keys
import           Yi.MiniBuffer            (commentRegion)
import           Yi.Misc                  (selectAll)
import           Yi.Rectangle             (getRectangle, killRectangle, yankRectangle)
import qualified Yi.Rope                  as R (YiString, length, singleton, withText)
import           Yi.String                (lines', unlines')
import           Yi.Keymap.Emacs.KillRing (clipboardToKillring, killringToClipboard)

customizedCuaKeymapSet :: Keymap -> KeymapSet
customizedCuaKeymapSet userKeymap =
    modelessKeymapSet $ selfInsertKeymap
                    <|> move
                    <|> select
                    <|> rect
                    <|> userKeymap
                    <|> other ctrl

keymap :: KeymapSet
keymap = portableKeymap ctrl

-- | Introduce a keymap that is compatible with both windows and osx,
--   by parameterising the event modifier required for commands
portableKeymap :: (Event -> Event) -> KeymapSet
portableKeymap cmd = modelessKeymapSet $ selfInsertKeymap <|> move <|> select <|> rect <|> other cmd

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  let action = (withCurrentBuffer . replaceSel $ R.singleton c) :: EditorM ()
  write action

setMark :: Bool -> BufferM ()
setMark b = do
  isSet <- use highlightSelectionA
  rectangleSelectionA .= b
  unless isSet $ do
       highlightSelectionA .= True
       pointB >>= setSelectionMarkPointB

unsetMark :: BufferM ()
unsetMark = highlightSelectionA .= False

replaceSel :: R.YiString -> BufferM ()
replaceSel s = do
  hasSel <- use highlightSelectionA
  if hasSel
    then getSelectRegionB >>= flip replaceRegionB s
    else insertN s

deleteSel :: BufferM () -> YiM ()
deleteSel act = do
  haveSelection <- withCurrentBuffer $ use highlightSelectionA
  if haveSelection
    then withEditor del
    else withCurrentBuffer act

cut :: YiM ()
cut = copy >> withEditor del

del :: EditorM ()
del = do
  asRect <- withCurrentBuffer $ use rectangleSelectionA
  if asRect
    then killRectangle
    else withCurrentBuffer $ deleteRegionB =<< getSelectRegionB

copy :: YiM ()
copy = do
  text <- withCurrentBuffer $ do
     asRect <- use rectangleSelectionA
     if not asRect
       then getSelectRegionB >>= readRegionB
       else do
         (reg, l, r) <- getRectangle
         let dropOutside = fmap (T.take (r - l) . T.drop l)
         R.withText (unlines' . dropOutside . lines') <$> readRegionB reg
  withEditor $ setRegE text
  killringToClipboard

paste :: YiM ()
paste = do
  clipboardToKillring
  asRect <- withCurrentBuffer (use rectangleSelectionA)
  withEditor $
    if asRect then yankRectangle
    else withCurrentBuffer . replaceSel =<< getRegE

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
 (spec KLeft          , moveB Character Backward),
 (spec KPageUp        , scrollScreensB (-1)),
 (spec KPageDown      , scrollScreensB 1)
 ]


move, select, rect :: Keymap
other :: (Event -> Event) -> Keymap

move   = choice [            k  ?>>! unsetMark       >> a | (k,a) <- moveKeys]
select = choice [      shift k  ?>>!   setMark False >> a | (k,a) <- moveKeys]
rect   = choice [meta (shift k) ?>>!   setMark True  >> a | (k,a) <- moveKeys]
other  cmd = choice [
 spec KBS         ?>>! deleteSel bdeleteB,
 spec KDel        ?>>! deleteSel (deleteN 1),
 spec KEnter      ?>>! replaceSel $ R.singleton '\n',
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
 cmd (char '/')   ?>>! commentRegion,
 cmd (char ']')   ?>>! autoIndentB IncreaseOnly,
 cmd (char '[')   ?>>! autoIndentB DecreaseOnly,
 cmd (char 'a')   ?>>! selectAll
 ]
