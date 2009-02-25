import Yi hiding (defaultConfig)
import Yi.Keymap.Emacs (keymap)
-- import Yi.Users.JP.Experimental (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim

import Control.Monad
import Data.Char
import Data.List (drop, length)
import Data.Maybe
import Data.Monoid
import Prelude ()
import Yi.Char.Unicode (greek, symbols)
import Yi.Hoogle
import Yi.Keymap.Keys
import Yi.Lexer.Alex (tokToSpan, Tok)
import Yi.Lexer.Haskell as Hask
import Yi.Mode.Haskell as Haskell
import Yi.Prelude
import Yi.String
import Yi.Syntax
import Yi.Syntax.Tree
import qualified Yi.Interact as I

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (' ':)

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (drop 1)

tokenToText :: Token -> Maybe String
tokenToText (Hask.ReservedOp Hask.BackSlash) = Just "λ"
tokenToText (Hask.ReservedOp Hask.RightArrow) = Just "→ " -- should be → in types and · in exprs
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
                        modeGetAnnotations = tokenBasedAnnots tta,

                        -- modeAdjustBlock = \_ _ -> return (),
                        -- modeGetStrokes = \_ _ _ _ -> [],
                        modeName = "my " ++ modeName mode,
                        -- example of Mode-local rebinding
                        modeKeymap = (choice [ctrlCh 'c' ?>> ctrlCh 'l' ?>>! ghciLoadBuffer,
                                              ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghciGet,
                                              ctrlCh 'c' ?>> ctrl (char 'h') ?>>! hoogle
                                             ]
                                      <||)  
                       }

noAnnots _ _ = []              

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

tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
tta = sequenceA . tokToSpan . (fmap Main.tokenToText)

parenIns :: Char -> Char -> BufferM ()
parenIns open close = do
    x <- readB
    if x == '\0' || isSpace x then insertN [open,close] >> leftB else insertN [open]

frontend :: UIBoot
Just frontend = foldr1 (<|>) $ fmap (flip lookup availableFrontends) ["cocoa", "vty"] 

defaultConfig = defaultEmacsConfig


main :: IO ()
main = yi $ defaultConfig {
                           -- configInputPreprocess = escToMeta,
                           startFrontEnd = frontend,
                           modeTable = AnyMode (haskellModeHooks Haskell.cleverMode) 
                                     : AnyMode (haskellModeHooks Haskell.fastMode) 
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

-- Just a stupid input preprocessor for testing. 
testPrep :: I.P Event Event
testPrep = mkAutomaton $ forever $ ((anyEvent >>= I.write) ||> do
    char 'C' ?>> I.write (Event (KASCII 'X') []))


-- Input preprocessor: Transform Esc;Char into Meta-Char
-- Useful for emacs lovers ;)
escToMeta :: I.P Event Event
escToMeta = mkAutomaton $ forever $ (anyEvent >>= I.write) ||> do
    event (spec KEsc)
    c <- printableChar
    I.write (Event (KASCII c) [MMeta])
