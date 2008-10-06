import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell as Haskell
import Data.List (drop)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys
import Yi.String
import Data.Char

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Line $ mapLines (' ':)

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Line $ mapLines (drop 1)

bestHaskellMode = Shim.minorMode $ Haskell.cleverMode 
                       -- Haskell.preciseMode
                       {
                        -- example of Mode-local rebinding
                        modeKeymap = (choice [ctrl (char 'c') ?>> ctrl(char 'c') ?>>! haskellToggleCommentSelectionB,
                                              ctrlCh 'c' ?>> char 'l' ?>>! ghciLoadBuffer,
                                              ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghciGet
                                             ]
                                      <||)  
                         -- uncomment this for Shim (dot is important!)
                         -- . modeKeymap Shim.mode
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
    = choice [pString ('\\':i) >>! insertN o | (i,o) <- greek] <|> -- greek letters, LaTeX-style
      choice [spec KEsc ?>> pString i >>! insertN o | (i,o) <- symbols] 

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
                           configKillringAccumulate = True,
                           modeTable = AnyMode bestHaskellMode : modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = Just 10 
                             -- , configTheme = darkBlueTheme
                             },
                           defaultKm = choice [extraInput, parensInput] <|| keymap
                              <|> (ctrl (char '>') ?>>! increaseIndent)
                              <|> (ctrl (char '<') ?>>! decreaseIndent)
                          }
