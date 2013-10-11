module Yi.Config.Users.Gwern (config) where

import Yi
import Yi.Hoogle (hoogle)
import Yi.Keymap.Emacs (defKeymap, eKeymap, mkKeymap)
import qualified Yi.Mode.Haskell as H
import qualified Yi.Mode.IReader as IReader (ireaderMode)

config :: Config
config = defaultEmacsConfig
 { modeTable = AnyMode bestHaskellMode : AnyMode IReader.ireaderMode : modeTable defaultConfig,
   -- Keymap Configuration
   defaultKm = myKeymap,
   configUI = defaultUIConfig {configFontSize = Nothing, configWindowFill = ' '} }
    where defaultUIConfig :: UIConfig
          defaultUIConfig = configUI defaultConfig
          -- bestHaskellMode :: Mode (Tree (Tok Token))
          bestHaskellMode = -- Shim.minorMode $
                             H.cleverMode { modeKeymap =
                                    topKeymapA %~ ((ctrlCh 'c' ?>> choice [ctrlCh 'l' ?>>! H.ghciLoadBuffer,
                                                              ctrl (char 'z') ?>>! H.ghciGet,
                                                              ctrl (char 'h') ?>>! hoogle,
                                                              ctrlCh 'r' ?>>! H.ghciSend ":r",
                                                              ctrlCh 't' ?>>! H.ghciInferType])
                                                   <||) }

myKeymap :: KeymapSet
myKeymap = mkKeymap $ override Yi.Keymap.Emacs.defKeymap $ \proto _self ->
   proto { eKeymap = eKeymap proto ||>
           -- Add a M-g binding for goto-line
           (metaCh 'g' ?>>! gotoLn)
           -- Use a more clever binding for Home
           ||> (spec KHome ?>>! moveNonspaceOrSol) }
