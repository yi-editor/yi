import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell as Haskell
import Data.List (drop, length)
import Yi.Char.Unicode (greek, symbols)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys
import Yi.String
import Data.Char
import Data.Monoid
import Yi.Hoogle
import Yi.Syntax.Haskell as Hask
import Yi.Lexer.Haskell as Hask
import Yi.Syntax.Paren as Paren
import Yi.Syntax.Tree
import Yi.Syntax.OnlineTree as OnlineTree
import Data.Maybe
import Yi.Lexer.Alex (tokToSpan)

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (' ':)

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (drop 1)

tokenToText :: Token -> Maybe String
tokenToText (Hask.ReservedOp Hask.BackSlash) = Just "λ"
-- tokenToText (Hask.ReservedOp Hask.RightArrow) = Just "→ " -- should be → in types and · in exprs
tokenToText (Hask.ReservedOp Hask.DoubleRightArrow) = Just "⇒ "
tokenToText (Hask.ReservedOp Hask.LeftArrow) = Just "← "
-- tokenToText (Hask.Operator ".") = Just "∘" -- should be · or . in types and ∘ in exprs
tokenToText (Hask.Operator "/=") = Just "≠"
tokenToText (Hask.Operator ">=") = Just "≥"
tokenToText (Hask.Operator "<=") = Just "≤"
tokenToText _ = Nothing

haskellModeHooks mode = 
                  -- uncomment for shim:
                  -- Shim.minorMode $ 
                     mode {
                        modeName = "my " ++ modeName mode,
                        -- example of Mode-local rebinding
                        modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! ghciLoadBuffer,
                                              ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghciGet,
                                              ctrlCh 'c' ?>> ctrl (char 'h') ?>>! hoogle
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

tta = sequenceA . tokToSpan . (fmap Main.tokenToText)

parenIns :: Char -> Char -> BufferM ()
parenIns open close = do
    x <- readB
    if x == '\0' || isSpace x then insertN [open,close] >> leftB else insertN [open]

Just frontend = foldr1 (<|>) $ fmap (flip lookup availableFrontends) ["cocoa", "vty"] 

main :: IO ()
main = yi $ defaultEmacsConfig {
                           startFrontEnd = frontend,
                           modeTable = AnyMode (haskellModeHooks Haskell.cleverMode) {modeGetAnnotations = \t begin -> catMaybes $ fmap tta $ concatMap toList t}
                                                                                        
                                     : AnyMode (haskellModeHooks Haskell.fastMode) {modeGetAnnotations = \t begin -> catMaybes $ fmap tta $ dropToIndex begin t}

                                     : modeTable defaultConfig,
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
