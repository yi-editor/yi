{-# LANGUAGE Rank2Types #-}
-- | A simple text mode; it does very little besides define a comment syntax.
-- We have it as a separate mode so users can bind the commands to this mode specifically.
module Yi.Mode.IReader where

import Yi.Buffer.HighLevel
import Yi.Buffer.Misc
import Yi.IReader
import Yi.Keymap.Keys
import Yi.Modes
import Yi.Syntax (Stroke)
import qualified Yi.Syntax.Linear as Linear

abstract :: forall syntax. Mode syntax
abstract = fundamentalMode { modeToggleCommentSelection = toggleCommentSelectionB "<!-- " "-->",
                             modeApplies = anyExtension ["irtxt"],
                             modeKeymap = ikeys }

ireaderMode :: Mode (Linear.Result Stroke)
ireaderMode = abstract { modeName = "text files for interactive reading" }

-- Default bindings.
ikeys = (choice  [metaCh '1' ?>>! saveAndNextArticle,
                  metaCh '2' ?>>! saveAsNewArticle,
                  metaCh '3' ?>>! deleteAndNextArticle] <||)

ireadMode ::  BufferM ()
ireadMode = setAnyMode (AnyMode ireaderMode)