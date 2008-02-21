--
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
--

--
-- Front end to the library, for use by external scripts. Just reexports
-- a bunch of modules.
--
-- You should therefore:
--      import Yi.Yi
-- in your ~/.yi/ scripts
--

module Yi.Yi (
              -- all things re-exported here are made available to keymaps definitions.

        module Control.Monad, -- since all actions are monadic, this is very useful to combine them.
        module Control.Applicative, -- same reasoning
        module Yi.Buffer,
        module Yi.Buffer.HighLevel,
        module Yi.Buffer.Normal,
        module Yi.Core,
        module Yi.Debug,
        module Yi.Dired,
        module Yi.Editor,
        module Yi.Eval,
        module Yi.Event, -- hack, for key defns
        module Yi.File,
        module Yi.Interact,
        module Yi.Buffer.Region,
        module Yi.Search,
        module Yi.Style,
        defaultPublishedActions
   ) where
import Control.Monad
import Control.Applicative
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Core
import Yi.Debug
import Yi.Dired
import Yi.Editor
import Yi.Eval
import Yi.Event -- so we can see key defns
import Yi.File
import Yi.Interact hiding (write)
import Yi.Buffer.Region
import Yi.Search
import Yi.Style
import Data.Map as M




-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("<expr>", makeAction "<expr>")
-- So we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

-- TODO: This should be a list of published symbols, of
-- arbitrary type; M-x shall conjure-up a small haskell-like
-- interpreter.

defaultPublishedActions :: M.Map String Action
defaultPublishedActions = M.fromList $ 
    [ ("leftB"                  , makeAction leftB) 
    , ("lineCommentSelectionB"  , makeAction lineCommentSelectionB)
    , ("unLineCommentSelectionB", makeAction unLineCommentSelectionB)
    , ("insertB '\t'"           , makeAction (insertB '\t'))
      -- I have added an action to insert a tab character
      -- because it is useful for makefiles.
    ]

