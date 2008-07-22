{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua  (keymap)
where

import Yi.Prelude
import Yi.Core
import Yi.Event
import Yi.Interact hiding (write)
import Yi.Keymap.Keys
import Yi.Keymap.Emacs.KillRing
import Yi.Keymap.Emacs.Utils
import Yi.Accessor
import Yi.Buffer
import Yi.Buffer.Normal
import Yi.Buffer.HighLevel
import Data.Char
import Prelude hiding (error)

import Control.Monad
import Control.Applicative

selfInsertKeymap :: Keymap
selfInsertKeymap = do
  c <- printableChar
  write (insertSelf c)

keymap :: Keymap
keymap = selfInsertKeymap <|> choice keys

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
        ctrl  (char 'x')    ?>>! killRegion,
        ctrl  (char 'c')    ?>>! killRingSaveE,
        ctrl  (char 'v')    ?>>! yankE
       ]



