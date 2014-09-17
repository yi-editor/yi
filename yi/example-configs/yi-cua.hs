import Yi

-- Import the desired UI as needed.
-- Some are not complied in, so we import none here.

-- import Yi.UI.Vty (start)
-- import Yi.UI.Pango (start)

myConfig :: Config
myConfig = defaultCuaConfig -- replace with defaultVimConfig or defaultCuaConfig

defaultUIConfig :: UIConfig
defaultUIConfig = configUI myConfig

-- Change the below to your needs, following the explanation in comments. See
-- module Yi.Config for more information on configuration. Other configuration
-- examples can be found in the examples directory. You can also use or copy
-- another user configuration, which can be found in modules Yi.Users.*

main :: IO ()
main = yi $ myConfig
  {

   -- Keymap Configuration
   defaultKm = defaultKm myConfig,

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
