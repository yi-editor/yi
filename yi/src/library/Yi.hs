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
    module Yi.Config,
    module Yi.Core,
    module Yi.Dired,
    module Yi.Eval,
    module Yi.File,
    module Yi.Config.Default,
    module Yi.Search,
    module Yi.Style,
    module Yi.Style.Library,
    module Yi.Misc,
    module Yi.Mode.Haskell,
    module Yi.Mode.IReader,
  ) where

import Data.Prototype
import Yi.Boot
import Yi.Config
import Yi.Core
import Yi.Dired
import Yi.Eval
import Yi.File
import Yi.Config.Default
import Yi.Search
import Yi.Style
import Yi.Style.Library
import Yi.Misc
import Yi.Mode.Haskell (ghciGet, ghciLoadBuffer,
                        ghciSetProcessName, ghciSetProcessArgs)
import Yi.Mode.IReader (ireaderMode, ireadMode)
