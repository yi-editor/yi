-- Copyright (c) 2008 Jean-Philippe Bernardy
{-# LANGUAGE FlexibleContexts #-}

module Yi.Keymap.Keys where
-- * Combinators for building keymaps.

import Yi.Event
import Yi.Keymap
import Data.Char
import Prelude hiding (error)
import Yi.Interact hiding (write)
import Control.Monad (when)
import Yi.Debug

printableChar :: (MonadInteract m w Event) => m Char
printableChar = do
  Event (KASCII c) [] <- anyEvent
  when (not $ isPrint c) $ 
       fail "unprintable character"
  return c


charOf :: (MonadInteract m w Event) => (Event -> Event) -> Char -> Char -> m Char
charOf modifier l h = 
    do Event (KASCII c) [] <- eventBetween (modifier $ char l) (modifier $ char h)
       return c

shift,ctrl,meta :: Event -> Event
shift (Event (KASCII c) ms) | isAlpha c = Event (KASCII (toUpper c)) ms
                           | otherwise = error "shift: unhandled event"
shift (Event k ms) = Event k (MShift:ms)

ctrl (Event k ms) = Event k (MCtrl:ms)

meta (Event k ms) = Event k (MMeta:ms)

char :: Char -> Event
char '\t' = Event KTab []
char c = Event (KASCII c) []

-- | Convert a special key into an event
spec :: Key -> Event
spec k = Event k []


(>>!) :: (MonadInteract m Action Event, YiAction a x, Show x) => m b -> a -> m ()
p >>! act = p >> write act

(?>>) :: (MonadInteract m action Event) => Event -> m a -> m a
ev ?>> proc = event ev >> proc

(?>>!) :: (MonadInteract m Action Event, YiAction a x, Show x) => Event -> a -> m ()
ev ?>>! act = event ev >> write act

infixl 1 >>!
infixr 0 ?>>!
infixr 0 ?>>
