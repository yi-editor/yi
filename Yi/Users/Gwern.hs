import Yi  hiding ((.))
import Yi.Buffer.HighLevel
import Yi.Buffer.Misc
import Yi.Hoogle (hoogle)
import Yi.Keymap.Emacs (defKeymap, eKeymap, mkKeymap)
import Yi.Misc
import Yi.Mode.Haskell as Haskell
import Yi.Mode.IReader as IReader
import Yi.Mode.Shim    as Shim

main :: IO ()
main = yi $ defaultEmacsConfig
 { modeTable = AnyMode bestHaskellMode : AnyMode IReader.ireaderMode : modeTable defaultConfig,
   -- Keymap Configuration
   defaultKm = myKeymap,
   configUI = defaultUIConfig {configFontSize = Nothing, configWindowFill = ' '}}
    where defaultUIConfig :: UIConfig
          defaultUIConfig = configUI defaultConfig

          bestHaskellMode = Shim.minorMode $ 
           Haskell.cleverMode { modeKeymap = 
                                (choice [ctrlCh 'c' ?>> ctrl (char 'l') ?>>! ghciLoadBuffer,
                                         -- Insert a type, go back up a line, and indent it properly.
                                         -- Good for 'where' expressions
                                         ctrlCh 'c' ?>> ctrl (char 't') ?>>! (annotType 
                                                                              >> (withBuffer $ moveB VLine Backward 
                                                                                  >> adjIndent IncreaseCycle)),
                                         -- Complete the word at point with Hoogle
                                         ctrlCh 'c' ?>> ctrl (char 'h') ?>>! hoogle,
                                         -- Use a more clever binding for Home
                                         spec KHome ?>>! moveNonspaceOrSol] <||) }

-- Add a M-g binding for goto-line
myKeymap :: KeymapSet
myKeymap = mkKeymap $ override Yi.Keymap.Emacs.defKeymap $ \proto _self -> 
   proto { eKeymap = eKeymap proto ||> (metaCh 'g' ?>>! gotoLn) }