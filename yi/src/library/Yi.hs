{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Facade of the Yi library, for use by confguration file. Just
-- re-exports a bunch of modules.
--
-- You should therefore:
--
-- @     import Yi@
--
-- in your @~/.config/yi/yi.hs@.

module Yi
  (
    module Data.Prototype, -- prototypes are mainly there for config; makes sense to export them.
    module Yi.Boot,
    module Yi.Buffer,
    module Yi.Config,
    module Yi.Config.Default,
    module Yi.Core,
    module Yi.Dired,
    module Yi.Editor,
    module Yi.Eval,
    module Yi.File,
    module Yi.Keymap,
    module Yi.Keymap.Keys,
    module Yi.Misc,
    module Yi.Mode.Haskell,
    module Yi.Mode.IReader,
    module Yi.Search,
    module Yi.Style,
    module Yi.Style.Library,
  ) where

import Data.Prototype
import Yi.Boot
import Yi.Buffer
import Yi.Config
import Yi.Config.Default
import Yi.Core
import Yi.Dired
import Yi.Editor
import Yi.Eval
import Yi.File
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Misc
import Yi.Mode.Haskell (ghciGet, ghciLoadBuffer,
                        ghciSetProcessName, ghciSetProcessArgs)
import Yi.Mode.IReader (ireaderMode, ireadMode)
import Yi.Search
import Yi.Style
import Yi.Style.Library