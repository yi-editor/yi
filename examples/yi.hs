import Yi

-- Preamble
import Yi.Prelude
import Prelude ()

-- Import the desired keymap "template":
import Yi.Keymap.Emacs (keymap)
import Yi.Keymap.Cua (keymap)
import Yi.Keymap.Vim (keymap)

-- Import the desired UI as needed. 
-- Some are not complied in, so we import none here.

-- import Yi.UI.Vty (start)
-- import Yi.UI.Gtk (start)
-- import Yi.UI.Cocoa (start)
-- import Yi.UI.Pango (start)


defaultUIConfig = configUI defaultConfig

-- Change the below to your needs, following the explanation in comments. See
-- module Yi.Config for more information on configuration. Other configuration
-- examples can be found in the examples directory. You can also use or copy
-- another user configuration, which can be found in modules Yi.Users.*

main :: IO ()
main = yi $ defaultConfig 
  {
   
   -- Keymap Configuration
   defaultKm = Yi.Keymap.Emacs.keymap,   -- Replace Emacs by Vim or Cua as needed.
   configKillringAccumulate = True,      -- Should be True for emacs, False for others.

   -- UI Configuration
   -- Override the default UI as such: 
   startFrontEnd = startFrontEnd defaultConfig,
                    -- Yi.UI.Vty.start -- for Vty
   -- (can be overridden at the command line)
   -- Options:
   configUI = defaultUIConfig
     { 
       configFontSize = Nothing,
                        -- 'Just 10' for specifying the size.
       configStyle = configStyle defaultUIConfig,
                     -- darkBlueTheme  -- Change the color scheme here.
       
       configWindowFill = ' ' 
                          -- '~'    -- Typical for Vim
     }
  }

