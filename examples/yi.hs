import Yi

-- Preamble
import Yi.Prelude
import Prelude ()

-- Import the desired keymap "template":
import Yi.Keymap.Emacs (keymap)
import Yi.Keymap.Cua (keymap)
import Yi.Keymap.Vim (keymap)

-- Import the desired UI
-- import Yi.UI.Vty (start)
-- import Yi.UI.Gtk (start)
-- import Yi.UI.Cocoa (start)
-- import Yi.UI.Pango (start)


main :: IO ()
main = yi $ defaultConfig 
  {
   -- Override the default UI as such: (can also be chosen on the command line)
   -- startFrontEnd = Yi.UI.Gtk.start,
   
   -- Keymap to use:
   defaultKm = Yi.Keymap.Emacs.keymap,
   configKillringAccumulate = True,    -- Should be True for emacs, False for others.

   -- UI options:
   configUI = (configUI defaultConfig) 
     { 
       configFontSize = Nothing        -- 'Just 10' for specifying the size.
     -- , configStyle = darkBlueTheme  -- Change the color scheme here.
     -- , configWindowFill = '~'       -- Typical for Vim
     }
  }

  -- See the documentation of Yi.Config for more information on
  -- configuration.  Other configuration examples can be found in the
  -- examples directory; you can also use or copy another user
  -- configuration, which can be found in modules Yi.Users.*
