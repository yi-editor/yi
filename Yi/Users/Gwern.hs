-- import Yi.UI.Gtk (start)
-- import Yi.UI.Vty (start)

import Yi 
import Yi.Buffer.HighLevel
import Yi.Buffer.Misc
import Yi.Keymap.Emacs (keymap)
import Yi.Mode.Haskell as Haskell
import Yi.Mode.IReader as IReader

defaultUIConfig :: UIConfig
defaultUIConfig = configUI defaultConfig

main :: IO ()
main = yi $ defaultConfig
  { modeTable = AnyMode bestHaskellMode : AnyMode IReader.ireaderMode : modeTable defaultConfig,

   -- Keymap Configuration
   defaultKm = Yi.Keymap.Emacs.keymap -- Override M-g g, for shorter M-g binding.
                                <|> (metaCh 'g' ?>>! gotoLn),
   configKillringAccumulate = True,      -- Should be True for emacs, False for others.

   -- UI Configuration
   -- Override the default UI as such:
   startFrontEnd = startFrontEnd defaultConfig,
   configUI = defaultUIConfig {configFontSize = Nothing, configTheme = configTheme defaultUIConfig, configWindowFill = ' '}}
    where -- bestHaskellMode :: Mode (Yi.Syntax.Paren.Expr (Yi.Lexer.Alex.Tok Yi.Lexer.Haskell.Token))
          bestHaskellMode = Haskell.cleverMode { modeKeymap = (choice [ctrlCh 'c' ?>> ctrl (char 'l') ?>>! ghciLoadBuffer,
                                                                       -- Use a more clever binding for Home
                                                                       spec KHome ?>>! moveNonspaceOrSol] <||) }
