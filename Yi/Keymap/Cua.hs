{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy

module Yi.Keymap.Cua  (keymap)
where

import Yi.Yi
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

(?>) :: (MonadInteract m Action Event, YiAction a x, Show x) => Event -> a -> m ()
ev ?> act = event ev >> write act

infixr 0 ?>

keys :: [Keymap]
keys = [
        fun_ KEsc   ?> quitEditor,
        fun_ KRight ?> rightB,
        fun_ KLeft  ?> leftB,
        fun_ KUp    ?> moveB VLine Backward,
        fun_ KDown  ?> moveB VLine Forward,
        shi_ (fun_ KRight) ?> setMark >> rightB,
        ctr_ (key_ 'x') ?> killRegionE
       ]



shi_,ctr_ :: Event -> Event
shi_ (Event (KASCII c) ms) | isAlpha c = Event (KASCII (toUpper c)) ms
                           | otherwise = error "shi_: unhandled event"
shi_ (Event k ms) = Event k (MShift:ms)

ctr_ (Event k ms) = Event k (MCtrl:ms)

key_ :: Char -> Event
key_ c = Event (KASCII c) []

fun_ :: Key -> Event
fun_ k = Event k []
