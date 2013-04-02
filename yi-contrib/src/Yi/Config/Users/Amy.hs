import Yi

import Prelude ()
import Yi.Prelude

-- Import the desired UI as needed. 
-- Some are not complied in, so we import none here.

-- import Yi.UI.Vty (start)
-- import Yi.UI.Cocoa (start)
-- import Yi.UI.Pango (start)

import Yi.Keymap.Cua

myConfig = defaultCuaConfig

defaultUIConfig = configUI myConfig

-- Add M-x (which is probably Alt-x on your system) to the default
-- keyset, and have it launch our custom macro.
extendedCuaKeymapSet = customizedCuaKeymapSet $
  choice [
    metaCh 'x' ?>>! helloWorld
  ]

-- A custom macro
helloWorld :: YiM ()
helloWorld = withBuffer $ insertN "Hello, world!"


main :: IO ()
main = yi $ myConfig
  {
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


