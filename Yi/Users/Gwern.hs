import Yi  hiding ((.))
import Yi.Buffer.HighLevel
import Yi.Buffer.Misc
import Yi.Keymap.Emacs (keymap)
import Yi.Misc
import Yi.Mode.Haskell as Haskell
import Yi.Mode.IReader as IReader
import Yi.Mode.Shim    as Shim

main :: IO ()
main = yi $ defaultConfig
 { modeTable = AnyMode bestHaskellMode : AnyMode IReader.ireaderMode : modeTable defaultConfig,

   -- Keymap Configuration
    defaultKm = Yi.Keymap.Emacs.keymap -- Override M-g g, for shorter M-g binding.
                                <|> (metaCh 'g' ?>>! gotoLn),
   configKillringAccumulate = True,      -- Should be True for emacs, False for others.

   configUI = defaultUIConfig {configFontSize = Nothing, configWindowFill = ' '}}
    where defaultUIConfig :: UIConfig
          defaultUIConfig = configUI defaultConfig

          bestHaskellMode = Shim.minorMode $ 
           Haskell.cleverMode { modeKeymap = 
                                (choice [ctrlCh 'c' ?>> ctrl (char 'l') ?>>! ghciLoadBuffer,
                                         -- Insert a type, go back up a line, and indent it properly
                                         -- good for 'where' expressions
                                         ctrlCh 'c' ?>> ctrl (char 't') ?>>! (annotType 
                                                                              >> (withBuffer $ moveB VLine Backward 
                                                                                  >> adjIndent IncreaseCycle)),
                                         -- Use a more clever binding for Home
                                         spec KHome ?>>! moveNonspaceOrSol] <||) }
