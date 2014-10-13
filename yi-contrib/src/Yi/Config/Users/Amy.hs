{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Users.Amy
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Config.Users.Amy where

import Yi
import Yi.Keymap.Cua

-- Import the desired UI as needed.
-- Some are not complied in, so we import none here.

-- import Yi.UI.Vty (start)
-- import Yi.UI.Pango (start)

myConfig :: Config
myConfig = defaultCuaConfig {
    -- Keymap Configuration
    defaultKm = extendedCuaKeymapSet,

    -- UI Configuration
    -- Override the default UI as such:
    startFrontEnd = startFrontEnd myConfig,
                     -- Yi.UI.Vty.start -- for Vty
    -- (can be overridden at the command line)
    -- Options:
    configUI = defaultUIConfig
      {
        configFontSize = Nothing,
                         -- 'Just 10' for specifying the size.
        configTheme = configTheme defaultUIConfig,
                      -- darkBlueTheme  -- Change the color scheme here.

        configWindowFill = ' '
      }
  }

defaultUIConfig :: UIConfig
defaultUIConfig = configUI myConfig

-- Add M-x (which is probably Alt-x on your system) to the default
-- keyset, and have it launch our custom macro.
extendedCuaKeymapSet :: KeymapSet
extendedCuaKeymapSet = customizedCuaKeymapSet $
  choice [ metaCh 'x' ?>>! helloWorld ]

-- A custom macro
helloWorld :: YiM ()
helloWorld = withCurrentBuffer $ insertN "Hello, world!"
