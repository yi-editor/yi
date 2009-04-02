{-# LANGUAGE CPP, TypeFamilies #-}
import Yi hiding (defaultConfig)
import Yi.Keymap.Emacs (mkKeymap, defKeymap, ModeMap(..))
-- import Yi.Users.JP.Experimental (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim

import Data.List (drop, length)
import Data.Monoid
import Prelude ()
import Yi.Char.Unicode
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

osx :: Bool
#ifdef darwin_HOST_OS
osx = True
#else
osx = False
#endif


tokenToText :: Token -> Maybe String
tokenToText (Hask.ReservedOp Hask.BackSlash) = Just "λ"
tokenToText (Hask.ReservedOp Hask.RightArrow) = Just "→" -- should be → in types and · in exprs
tokenToText (Hask.ReservedOp Hask.DoubleRightArrow) = Just $ if osx then "⇒ " else "⇒"
tokenToText (Hask.ReservedOp Hask.LeftArrow) = Just $ if osx then "← " else "←"
-- tokenToText (Hask.Operator ".") = Just "∘" -- should be · or . in types and ∘ in exprs
tokenToText (Hask.Operator "/=") = Just "≠"
tokenToText (Hask.Operator ">=") = Just "≥"
tokenToText (Hask.Operator "<=") = Just "≤"
tokenToText (Hask.Operator "&&") = Just "∧"
tokenToText (Hask.Operator "||") = Just "∨"
tokenToText _ = Nothing


haskellModeHooks :: (Tok Token ~ Element syntax, SubTree syntax) =>Mode syntax -> Mode syntax
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

-- noAnnots _ _ = []              

mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod xs = choice [pString i >> adjustPriority (negate (length i)) >>! insertN o | (i,o) <- xs] 

extraInput :: Keymap
extraInput 
    = spec KEsc ?>> mkInputMethod (greek ++ symbols ++ subscripts)


tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
tta = sequenceA . tokToSpan . (fmap Main.tokenToText)

frontend :: UIBoot
frontendName :: String
Just (frontendName, frontend) = foldr1 (<|>) $ fmap (\nm -> find ((nm ==) . fst) availableFrontends) ["cocoa", "vty"] 

isCocoa :: Bool
isCocoa = frontendName == "cocoa"


defaultConfig :: Config
defaultConfig = defaultEmacsConfig


myKeymap :: Keymap
myKeymap = mkKeymap $ override defKeymap $ \proto _self -> proto {completionCaseSensitive = True}

main :: IO ()
main = yi $ defaultConfig {
                           configInputPreprocess = I.idAutomaton,
                           startFrontEnd = frontend,
                           modeTable = AnyMode (haskellModeHooks Haskell.cleverMode) 
                                     : AnyMode (haskellModeHooks Haskell.fastMode) 
                                     : AnyMode (haskellModeHooks Haskell.literateMode) 
                                     : modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = if isCocoa then Just 12 else Just 10
                               -- , configTheme = darkBlueTheme
                             , configTheme = defaultLightTheme `override` \superTheme _ -> superTheme
                               {
                                 selectedStyle = Endo $ \a -> a { 
                                                                  foreground = white,
                                                                  background = black
                                                                }
                               }
                              -- , configFontName = Just "Monaco"
                             },
                           defaultKm = (adjustPriority (-1) >> choice [extraInput]) <|| myKeymap
                              <|> (ctrl (char '>') ?>>! increaseIndent)
                              <|> (ctrl (char '<') ?>>! decreaseIndent)
                          }

