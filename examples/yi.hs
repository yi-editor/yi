import Yi
import Yi.Keymap.Emacs (keymap)
-- You can use other keymap by importing some other module:
-- import  Yi.Keymap.Cua (keymap)

-- If configured with ghcAPI, Shim Mode can be enabled:
-- import qualified Yi.Mode.Shim as Shim
import Yi.Mode.Haskell as Haskell
import Data.List (isSuffixOf, drop)
import Yi.Prelude
import Prelude ()
import Yi.Keymap.Keys
import Yi.String

increaseIndent :: BufferM ()
increaseIndent = modifyExtendedSelectionB Line $ mapLines (' ':)

decreaseIndent :: BufferM ()
decreaseIndent = modifyExtendedSelectionB Line $ mapLines (drop 1)

myModetable :: ReaderT String Maybe AnyMode
myModetable = ReaderT $ \fname -> case () of 
                        _ | ".hs" `isSuffixOf` fname -> Just $ AnyMode bestHaskellMode
                        _ ->  Nothing
    where bestHaskellMode = Haskell.cleverMode 
                            -- Haskell.preciseMode
                            {
                             -- example of Mode-local rebinding
                             modeKeymap = (choice [ctrl (char 'c') ?>> ctrl(char 'c') ?>>! haskellToggleCommentSelectionB,
                                                   ctrlCh 'c' ?>> char 'l' ?>>! ghciLoadBuffer,
                                                   ctrlCh 'c' ?>> ctrl (char 'z') ?>>! ghci
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
-- arrows: ↢ ↣ ↝ ↜  ↔ ⇤ ⇥ ⇸ ⇆

-- set: ∅ ∉ ∈ ⊇ ⊃
-- relations: ≝ ≤ ≥
-- circled operators: ⊕⊖⊗⊘ ⊙⊚⊛⊜⊝⍟ ⎊⎉
-- squared operators: ⊞⊟⊠⊡ 
-- turnstyles: ⊢⊣⊤⊥⊦⊧⊨⊩⊬⊭
-- parens: ⟪ ⟫



extraInput :: Keymap
extraInput 
    = choice [pString ('\\':i) >>! insertN o | (i,o) <- greek] <|> -- greek letters, LaTeX-style
      choice [pString ('`':i) >>! insertN o | (i,o) <- symbols] 

main :: IO ()
main = yi $ defaultConfig {
                           configKillringAccumulate = True,
                           modeTable = myModetable <|> modeTable defaultConfig,
                           configUI = (configUI defaultConfig) 
                             { configFontSize = Just 10 
                             -- , configStyle = darkBlueTheme
                             },
                           defaultKm = extraInput <|| keymap
                              <|> (ctrl (char '>') ?>>! increaseIndent)
                              <|> (ctrl (char '<') ?>>! decreaseIndent)
                          }
