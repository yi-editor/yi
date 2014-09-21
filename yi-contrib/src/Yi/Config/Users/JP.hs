{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Config.Users.JP
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Config.Users.JP (config) where

-- import Yi.Users.JP.Experimental (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim

import           Control.Applicative
import           Control.Lens
import           Data.Foldable (Foldable,find)
import           Data.Monoid
import           Data.Traversable (sequenceA)
import           Yi hiding (defaultConfig)
import           Yi.Hoogle
import qualified Yi.Interact as I
import           Yi.Keymap.Emacs (mkKeymap, defKeymap, ModeMap(..))
import           Yi.Lexer.Alex (tokToSpan, Tok)
import           Yi.Lexer.Haskell as Hask
import           Yi.Mode.Haskell as Haskell
import qualified Yi.Rope as R
import           Yi.String
import           Yi.Syntax
import           Yi.Syntax.Tree

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (R.cons ' ')

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Yi.Line $ mapLines (R.drop 1)

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
                        modeName = "my " <> modeName mode,
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

mkInputMethod :: [(String, R.YiString)] -> Keymap
mkInputMethod xs = choice [pString i >> adjustPriority (negate (length i)) >>! insertN o | (i,o) <- xs]

extraInput :: Keymap
extraInput
    = spec KEsc ?>> mkInputMethod (greek <> symbols <> subscripts)


tta :: Yi.Lexer.Alex.Tok Token -> Maybe (Yi.Syntax.Span String)
tta = sequenceA . tokToSpan . (fmap Yi.Config.Users.JP.tokenToText)

frontend :: UIBoot
Just (_, frontend) = foldr1 (<|>) $ fmap (\nm -> find ((nm ==) . fst) availableFrontends) ["vty"]

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
           _completionCaseSensitive = True,
           _eKeymap = (adjustPriority (-1) >> choice [extraInput]) <|| (fixKeymap <|| _eKeymap proto)
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
                             { configFontSize = Just 10
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

greek :: [(String, R.YiString)]
greek = [("alpha", "α")
        ,("'a", "α")
        ,("beta", "β")
        ,("'b", "β")
        ,("gamma", "γ")
        ,("'g", "γ")
        ,("Gamma", "Γ")
        ,("'G", "Γ")
        ,("delta", "δ")
        ,("'d", "δ")
        ,("Delta", "Δ")
        ,("'D", "Δ")
        ,("epsilon", "ε")
        ,("'z", "ζ")
        ,("zeta", "ζ")
        ,("'z", "ζ")
        ,("eta", "η")
        ,("theta", "θ")
        ,("Theta", "Θ")
        ,("iota", "ι")
        ,("'i", "ι")
        ,("kapa", "κ")
        ,("'k", "κ")
        ,("lambda", "λ")
        ,("'l", "λ")
        ,("Lambda", "Λ")
        ,("'L", "Λ")
        ,("mu", "μ")
        ,("'m", "μ")
        ,("nu", "ν")
        ,("'n", "ν")
        ,("xi", "ξ")
        ,("'x", "ξ")
        ,("omicron", "ο")
        ,("'o", "ο")
        ,("pi", "π")
        ,("Pi", "Π")
        ,("rho", "ρ")
        ,("'r", "ρ")
        ,("sigma", "σ")
        ,("'s", "σ")
        ,("Sigma", "Σ")
        ,("'S", "Σ")
        ,("tau", "τ")
        ,("'t", "τ")
        ,("phi", "φ")
        ,("Phi", "Φ")
        ,("chi", "χ")
        ,("Chi", "Χ")
        ,("psi", "ψ")
        ,("Psi", "Ψ")
        ,("omega", "ω")
        ,("'w", "ω")
        ,("Omega", "Ω")
        ,("'O", "Ω")
        ]

symbols :: [(String, R.YiString)]
symbols =
 [
 -- parens
  ("<","⟨")
 ,(">","⟩")
 ,(">>","⟫")
 ,("<<","⟪")

 ,("[[","⟦")
 ,("]]","⟧")

 -- quantifiers
 ,("forall", "∀")
 ,("exists", "∃")

 -- operators
 ,("<|","◃")
 -- ,("<|","◁") alternative
 ,("|>","▹")
 ,("v","∨")
 ,("u","∪")
 ,("V","⋁")
 ,("^","∧")
 ,("o","∘")
 ,(".","·")
 ,("x","×")
 ,("neg","¬")

 --- arrows
 ,("<-","←")
 ,("->","→")
 ,("|->","↦")
 ,("<-|","↤")
 ,("<--","⟵")
 ,("-->","⟶")
 ,("|-->","⟼")
 ,("==>","⟹")
 ,("=>","⇒")
 ,("<=","⇐")
 ,("~>","↝")
 ,("<~","↜")
 ,("<-<", "↢")
 ,(">->", "↣")
 ,("<->", "↔")
 ,("|<-", "⇤")
 ,("->|", "⇥")

 --- relations
 ,("c=","⊆")
 ,("c","⊂")
 ,("c-","∈")
 ,("/c-","∉")
 ,(">=","≥")
 ,("=<","≤")

 ---- equal signs
 ,("=def","≝")
 ,("=?","≟")
 ,("=-","≡")
 ,("~=","≃")
 ,("/=","≠")

 -- misc
 ,("_|_","⊥")
 ,("Top","⊤")
 ,("|N","ℕ")
 ,("|P","ℙ")
 ,("|R","ℝ")
 ,("^n","ⁿ")
 ,("::","∷")
 ,("0", "∅")
 ,("*", "★") -- or "⋆"

 -- dashes
 ,("-","−")

 -- quotes
 ,("\"","“”")

 -- turnstyles
 ,("|-", "⊢")
 ,("|/-", "⊬")
 ,("-|", "⊣")
 ,("|=", "⊨")
 ,("|/=", "⊭")
 ,("||-", "⊩")

 ]

-- More:
-- arrows: ⇸ ⇆
-- set:  ⊇ ⊃
-- circled operators: ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⍟  ⎊ ⎉
-- squared operators: ⊞ ⊟ ⊠ ⊡
-- turnstyles: ⊦ ⊧


subscripts :: [(String, R.YiString)]
subscripts = zip (fmap (('_':). (:[])) "0123456789+-=()")
                 (fmap R.singleton "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎")
