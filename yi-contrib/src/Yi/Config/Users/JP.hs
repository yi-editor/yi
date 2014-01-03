{-# LANGUAGE CPP, TypeFamilies #-}
module Yi.Config.Users.JP (config) where

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
tokenToText (Hask.Operator "==") = Just "≡"
tokenToText (Hask.Operator ">=") = Just "≥"
tokenToText (Hask.Operator "<=") = Just "≤"
tokenToText (Hask.Operator "&&") = Just "∧"
tokenToText (Hask.Operator "||") = Just "∨"
tokenToText _ = Nothing


haskellModeHooks :: (Foldable tree) => Mode (tree (Tok Token)) -> Mode (tree (Tok Token))
haskellModeHooks mode = 
                  -- uncomment for shim:
                  -- Shim.minorMode $ 
                     mode {
                        modeGetAnnotations = tokenBasedAnnots tta,

                        -- modeAdjustBlock = \_ _ -> return (),
                        -- modeGetStrokes = \_ _ _ _ -> [],
                        modeName = "my " ++ modeName mode,
                        -- example of Mode-local rebinding
                        modeKeymap = topKeymapA %~ ((ctrlCh 'c' ?>> choice [ctrlCh 'l' ?>>! ghciLoadBuffer,
                                                              ctrl (char 'z') ?>>! ghciGet,
                                                              ctrl (char 'h') ?>>! hoogle,
                                                              ctrlCh 'r' ?>>! ghciSend ":r",
                                                              ctrlCh 't' ?>>! ghciInferType
                                                             ])
                                      <||)
                       }

-- noAnnots _ _ = []              

mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod xs = choice [pString i >> adjustPriority (negate (length i)) >>! insertN o | (i,o) <- xs] 

extraInput :: Keymap
extraInput 
    = spec KEsc ?>> mkInputMethod (greek ++ symbols ++ subscripts)


tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
tta = sequenceA . tokToSpan . (fmap Yi.Config.Users.JP.tokenToText)

frontend :: UIBoot
frontendName :: String
Just (frontendName, frontend) = foldr1 (<|>) $ fmap (\nm -> find ((nm ==) . fst) availableFrontends) ["cocoa", "vty"] 

isCocoa :: Bool
isCocoa = frontendName == "cocoa"


defaultConfig :: Config
defaultConfig = defaultEmacsConfig

deleteB' :: BufferM ()
deleteB' = adjBlock (-1) >> deleteN 1

-- restore sanity in 1-character deletes.
fixKeymap :: Keymap
fixKeymap =   choice [(ctrlCh 'd'           ?>>! (deleteB'))
                     , spec KBS             ?>>! ((adjBlock (-1) >> bdeleteB))
                     , spec KDel            ?>>! ((deleteB'))]
                      

myKeymap :: KeymapSet
myKeymap = mkKeymap $ override defKeymap $ \proto _self -> 
   proto {
           completionCaseSensitive = True,
           eKeymap = (adjustPriority (-1) >> choice [extraInput]) <|| (fixKeymap <|| eKeymap proto)
                     <|> (ctrl (char '>') ?>>! increaseIndent)
                     <|> (ctrl (char '<') ?>>! decreaseIndent)
         }

config :: Config
config = defaultConfig {
                           configInputPreprocess = I.idAutomaton,
                           startFrontEnd = frontend,
                           modeTable = AnyMode (haskellModeHooks Haskell.preciseMode) 
                                     : AnyMode (haskellModeHooks Haskell.cleverMode) 
                                     : AnyMode (haskellModeHooks Haskell.fastMode) 
                                     : AnyMode (haskellModeHooks Haskell.literateMode) 
                                     : modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = if isCocoa then Just 12 else Just 10
                               -- , configTheme = darkBlueTheme
                             , configTheme = defaultTheme `override` \superTheme _ -> superTheme
                               {
                                 selectedStyle = Endo $ \a -> a { 
                                                                  foreground = white,
                                                                  background = black
                                                                }
                               }
                              -- , configFontName = Just "Monaco"
                             },
                           defaultKm = myKeymap
                          }

