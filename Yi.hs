-- Copyright (c) 2007,8 JP Bernardy
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons

-- |
-- Facade of the Yi library, for use by confguration file. Just re-exports
-- a bunch of modules.
--
-- You should therefore:
--      import Yi.Yi
-- in your ~/.yi/ scripts
--

module Yi
  (
    -- all things re-exported here are made available to keymaps definitions.
    module Control.Monad, -- since all actions are monadic, this is very useful to combine them.
    module Control.Applicative, -- same reasoning
    module Yi.Boot,
    module Yi.Buffer,
    module Yi.Buffer.HighLevel,
    module Yi.Buffer.Normal,
    module Yi.Config,
    module Yi.Core,
    module Yi.Debug,
    module Yi.Dired,
    module Yi.Editor,
    module Yi.Eval,
    module Yi.File,
    module Yi.Main,
    module Yi.Buffer.Region,
    module Yi.Search,
    module Yi.Style,
    module Yi.Keymap.Keys,
   ) where

import Control.Applicative
import Control.Monad hiding (mapM_, mapM)
import Yi.Boot
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Normal
import Yi.Buffer.Region
import Yi.Config
import Yi.Core
import Yi.Debug
import Yi.Dired
import Yi.Editor
import Yi.Eval
import Yi.File
import Yi.Keymap.Keys
import Yi.Main (defaultConfig)
import Yi.Search
import Yi.Style
