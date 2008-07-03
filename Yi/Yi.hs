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
        module Yi.Config,
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
import Control.Monad hiding (mapM_, mapM)
import Control.Applicative
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Config
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
import Data.Dynamic
import Yi.Keymap.Emacs.Utils


-- | List of published Actions

-- THIS MUST BE OF THE FORM:
-- ("symbol", box symbol")
-- ... so we can hope getting rid of this someday.
-- Failing to conform to this rule exposes the code to instant deletion.

defaultPublishedActions :: M.Map String [Dynamic]
defaultPublishedActions = M.fromList $ 
    [ ("leftB"                  , box leftB) 
    , ("pointB"                 , box (fromPoint <$> pointB)) 
    , ("linePrefixSelectionB"  , box linePrefixSelectionB)
    , ("unLineCommentSelectionB", box unLineCommentSelectionB)
    , ("insertB"                , box insertB)
    , ("revertE"                , box revertE)
    , ("numberOfB"              , box numberOfB)
    , ("Character"              , box Character)
    , ("Line"                   , box Line)
    , ("Word"                   , box Word)
    , ("Paragraph"              , box Paragraph)
    , ("Document"               , box Document)
    , ("cabalConfigureE"        , box cabalConfigureE)
    , ("cabalBuildE"            , box cabalBuildE)
    , ("reloadProjectE"         , box reloadProjectE)
    , ("atBoundaryB"            , box atBoundaryB)
    , ("regionOfPartB"          , box regionOfPartB)
    , ("Forward"                , box Forward)
    , ("Backward"               , box Backward)
    , ("deleteBlankLinesB"      , box deleteBlankLinesB)
    , ("writeB"                 , box writeB)
    , ("getSelectRegionB"       , box getSelectRegionB)
    ]

  where box x = [toDyn x]
