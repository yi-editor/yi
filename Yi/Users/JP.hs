import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell as Haskell
import Data.List (drop, length)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys
import Yi.String
import Data.Char
import Yi.UI.Vty (start)

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
                        modeKeymap = (choice [ctrlCh 'c' ?>> char 'l' ?>>! ghciLoadBuffer,
                                              ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghciGet
                                             ]
                                      <||)  
                       }
greek :: [(String, String)]
greek = [("alpha", "α"),
         ("beta", "β"),
         ("gamma", "γ"),
         ("delta", "δ")
        ]

quotes = "“”"


symbols :: [(String, String)]
symbols = 
 [
 -- parens
  ("<","⟨")
 ,(">","⟩")
 ,("[[","⟦")
 ,("]]","⟧")
 
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

 --- relations
 ,("c=","⊆") 
 ,("c","⊂")    

 ---- equal signs
 ,("=def","≝")
 ,("=?","≟")
 ,("=-","≡")
 ,("~=","≃")
 ,("/=","≠")

 -- misc
 ,("_|_","⊥")
 ,("T","⊤")
 ,("|N","ℕ")
 ,("|P","ℙ")
 ,("^n","ⁿ")
 ,("::","∷")

 -- dashes
 ,("-","−")
 ]

mkInputMethod :: [(String,String)] -> Keymap
mkInputMethod list = choice [pString i >> adjustPriority (negate (length i)) >>! insertN o | (i,o) <- list] 


-- More:
-- arrows: ↢ ↣   ↔ ⇤ ⇥ ⇸ ⇆

-- set: ∅ ∉ ∈ ⊇ ⊃
-- relations: ≝ ≤ ≥
-- circled operators: ⊕⊖⊗⊘ ⊙⊚⊛⊜⊝⍟ ⎊⎉
-- squared operators: ⊞⊟⊠⊡ 
-- turnstyles: ⊢⊣⊤⊥⊦⊧⊨⊩⊬⊭
-- parens: ⟪ ⟫



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

main :: IO ()
main = yi $ defaultConfig {
                           startFrontEnd = Yi.UI.Vty.start,
                           configKillringAccumulate = True,
                           modeTable = AnyMode bestHaskellMode : modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = Just 12 
                              -- , configTheme = darkBlueTheme
                              -- , configFontName = Just "Monaco"
                             },
                           defaultKm = (adjustPriority (-1) >> choice [extraInput]) <|| keymap
                              <|> (ctrl (char '>') ?>>! increaseIndent)
                              <|> (ctrl (char '<') ?>>! decreaseIndent)
                          }
