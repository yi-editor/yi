-- Copyright (c) 2008 Jean-Philippe Bernardy
{-# LANGUAGE FlexibleContexts #-}

module Yi.Keymap.Keys where
-- * Combinators for building keymaps.

import Yi.Yi
import Data.Char
import Prelude hiding (error)


shift,ctrl,meta :: Event -> Event
shift (Event (KASCII c) ms) | isAlpha c = Event (KASCII (toUpper c)) ms
                           | otherwise = error "shift: unhandled event"
shift (Event k ms) = Event k (MShift:ms)

ctrl (Event k ms) = Event k (MCtrl:ms)

meta (Event k ms) = Event k (MMeta:ms)

char :: Char -> Event
char c = Event (KASCII c) []

-- | Convert a special key into an event
spec :: Key -> Event
spec k = Event k []

(?>) :: (MonadInteract m Action Event, YiAction a x, Show x) => Event -> a -> m ()
ev ?> act = event ev >> write act

infixr 0 ?>
