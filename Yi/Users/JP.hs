import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell as Haskell
import Data.List (drop, length)
import Yi.Char.Unicode (greek, symbols)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys
import Yi.String
import Data.Char
import Data.Monoid

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Line $ mapLines (' ':)

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Line $ mapLines (drop 1)

bestHaskellMode = 
                  -- uncomment for shim:
                  -- Shim.minorMode $ 
                  Haskell.cleverMode 
                       -- Haskell.preciseMode
                       {
                        -- example of Mode-local rebinding
                        modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! ghciLoadBuffer,
                                              ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghciGet
                                             ]
                                      <||)  
                       }
mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod list = choice [pString i >> adjustPriority (negate (length i)) >>! insertN o | (i,o) <- list] 

extraInput :: Keymap
extraInput 
    = spec KEsc ?>> mkInputMethod (greek ++ symbols)

parensInput :: Keymap
parensInput
    = choice [char open ?>>! parenIns open close | (open,close) <- 
              [('(',')'),
               ('[',']'),
               ('{','}')               
              ]
             ]

parenIns :: Char -> Char -> BufferM ()
parenIns open close = do
    x <- readB
    if x == '\0' || isSpace x then insertN [open,close] >> leftB else insertN [open]

Just frontend = foldr1 (<|>) $ fmap (flip lookup availableFrontends) ["cocoa", "vty"] 

main :: IO ()
main = yi $ defaultConfig {
                           startFrontEnd = frontend,
                           configKillringAccumulate = True,
                           modeTable = AnyMode bestHaskellMode : modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = Just 12 
                               -- , configTheme = darkBlueTheme
                             , configTheme = defaultLightTheme `override` \super _ -> super 
                               {
                                  selectedStyle = Endo $ \a -> a {foreground = white,
                                                                  background = black}
                               }
                              -- , configFontName = Just "Monaco"
                             },
                           defaultKm = (adjustPriority (-1) >> choice [extraInput]) <|| keymap
                              <|> (ctrl (char '>') ?>>! increaseIndent)
                              <|> (ctrl (char '<') ?>>! decreaseIndent)
                          }
