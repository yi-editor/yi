{-# LANGUAGE Rank2Types #-}
-- | A simple text mode; it does very little besides define a comment syntax.
-- We have it as a separate mode so users can bind the commands to this mode specifically.
module Yi.Mode.IReader where

import Yi.Buffer.Misc
import Yi.IReader
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Core (msgEditor)
import Yi.Modes (anyExtension, fundamentalMode)

abstract :: forall syntax. Mode syntax
abstract = fundamentalMode { modeApplies = anyExtension ["irtxt"],
                             modeKeymap = ikeys }
    where -- Default bindings.
          -- ikeys :: (MonadInteract f Yi.Keymap.Action Event) => f () -> f ()
          ikeys = (choice  [metaCh '1' ?>>! saveAndNextArticle,
                            metaCh '2' ?>>! saveAsNewArticle,
                            metaCh '3' ?>>! deleteAndNextArticle] <||)

ireaderMode :: Mode syntax
ireaderMode = abstract { modeName = "interactive reading of text" }

ireadMode ::  YiM ()
ireadMode = do withBuffer $ setAnyMode $ AnyMode ireaderMode
               nextArticle 
               msgEditor "M-1: next; M-2: save; M-3: delete"