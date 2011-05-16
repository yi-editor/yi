{-# LANGUAGE FlexibleContexts #-}
-- Copyright (c) 2008 Jean-Philippe Bernardy

-- | Combinators for building keymaps.

module Yi.Keymap.Keys 
    (
     module Yi.Event,
     module Yi.Interact,
     printableChar, charOf, shift, meta, ctrl, super, hyper, spec, char,
     (>>!), (>>=!), (?>>), (?>>!), (?*>>), (?*>>!),
     ctrlCh, metaCh, hyperCh,
     optMod,
     pString
    ) where

import Yi.Event
import Yi.Keymap
import Data.Char
import Prelude hiding (error)
import Yi.Interact hiding (write)
import Control.Monad (when)
import Yi.Debug
import Data.List (sort, nub)

printableChar :: (MonadInteract m w Event) => m Char
printableChar = do
  Event (KASCII c) [] <- anyEvent
  when (not $ isPrint c) $ 
       fail "unprintable character"
  return c

pString :: (MonadInteract m w Event) => String -> m [Event] 
pString = events . map char

charOf :: (MonadInteract m w Event) => (Event -> Event) -> Char -> Char -> m Char
charOf modifier l h = 
    do Event (KASCII c) _ <- eventBetween (modifier $ char l) (modifier $ char h)
       return c

shift,ctrl,meta,super,hyper :: Event -> Event
shift (Event (KASCII c) ms) | isAlpha c = Event (KASCII (toUpper c)) ms
                           | otherwise = error "shift: unhandled event"
shift (Event k ms) = Event k $ nub $ sort (MShift:ms)

ctrl (Event k ms) = Event k $ nub $ sort (MCtrl:ms)

meta (Event k ms) = Event k $ nub $ sort (MMeta:ms)

super (Event k ms) = Event k $ nub $ sort (MSuper:ms)

hyper (Event k ms) = Event k $ nub $ sort (MHyper:ms)

char :: Char -> Event
char '\t' = Event KTab []
char '\r' = Event KEnter []
char '\n' = Event KEnter []
char c = Event (KASCII c) []

ctrlCh :: Char -> Event
ctrlCh = ctrl . char

metaCh :: Char -> Event
metaCh = meta . char

hyperCh :: Char -> Event
hyperCh = hyper . char

-- | @optMod f ev@ produces a 'MonadInteract' that consumes @ev@ or @f ev@
optMod ::(MonadInteract m w Event) => (Event -> Event) -> Event -> m Event
optMod f ev = oneOf [ev, f ev]

-- | Convert a special key into an event
spec :: Key -> Event
spec k = Event k []


(>>!) :: (MonadInteract m Action Event, YiAction a x, Show x) => m b -> a -> m ()
p >>! act = p >> write act

(>>=!) :: (MonadInteract m Action Event, YiAction a x, Show x) => m b -> (b -> a) -> m ()
p >>=! act = p >>= write . act

(?>>) :: (MonadInteract m action Event) => Event -> m a -> m a
ev ?>> proc = event ev >> proc

(?>>!) :: (MonadInteract m Action Event, YiAction a x, Show x) => Event -> a -> m ()
ev ?>>! act = event ev >> write act

(?*>>) :: (MonadInteract m action Event) => [Event] -> m a -> m a
ev ?*>> proc = events ev >> proc

(?*>>!) :: (MonadInteract m Action Event, YiAction a x, Show x) => [Event] -> a -> m ()
ev ?*>>! act = events ev >> write act

infixl 1 >>!
infixl 1 >>=!
infixr 0 ?>>!
infixr 0 ?>>
infixr 0 ?*>>!
infixr 0 ?*>>
