{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua  (keymap)
where

import Yi.Yi
import Yi.Keymap.Emacs.Keys
import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.Utils
import Yi.Accessor
import Yi.Buffer
import Yi.Buffer.Normal
import Data.Char
import Prelude hiding (error)

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
  selfInsertKeymap <|> choice keys

setMark :: BufferM ()
setMark = do
  isSet <- getA highlightSelectionA
  when (not isSet) $ do
       setA highlightSelectionA True
       pointB >>= setSelectionMarkPointB

-- withBuffer (readRegionB =<< getSelectRegionB)


keys :: [Keymap]
keys = [
        spec KEsc           ?>>! quitEditor,
        spec KRight         ?>>! rightB,
        spec KLeft          ?>>! leftB,
        spec KUp            ?>>! moveB VLine Backward,
        spec KDown          ?>>! moveB VLine Forward,
        shift (spec KRight) ?>>! setMark >> rightB,
        ctrl  (char 'x')    ?>>! killRegionE
       ]



