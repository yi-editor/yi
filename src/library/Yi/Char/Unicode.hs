module Yi.Char.Unicode (greek, symbols, subscripts, superscripts, checkAmbs, disamb) where

import Control.Applicative (Applicative (pure))
import Data.List           (isPrefixOf)

{-# ANN module "HLint: ignore Use string literal" #-}

greek :: [(String, String)]
greek = [(name, unicode) | (_,name,unicode) <- greekData] ++
        [ ([leading,shorthand],unicode)
        | (Just shorthand,_,unicode) <- greekData
        , leading                    <- ['\'', 'g'] ]

-- | Triples: (shorthand, name, unicode)
greekData :: [(Maybe Char, String, String)]
greekData = [(Just 'a', "alpha", "α")
            ,(Just 'b', "beta", "β")
            ,(Just 'g', "gamma", "γ")
            ,(Just 'G', "Gamma", "Γ")
            ,(Just 'd', "delta", "δ")
            ,(Just 'D', "Delta", "Δ")
            ,(Just 'e' , "epsilon", "ε")
            ,(Just 'z', "zeta", "ζ")
            ,(Just 'N' , "eta", "η") -- N is close to n which is graphically close
            ,(Just 'E' , "eta", "η") -- E is close to e which is the start of eta
            ,(Nothing , "theta", "θ")
            ,(Nothing , "Theta", "Θ")
            ,(Just 'i', "iota", "ι")
            ,(Just 'k', "kapa", "κ")
            ,(Just 'l', "lambda", "λ")
            ,(Just 'L', "Lambda", "Λ")
            ,(Just 'm', "mu", "μ")
            ,(Just 'n', "nu", "ν")
            ,(Just 'x', "xi", "ξ")
            ,(Just 'o', "omicron", "ο")
            ,(Just 'p' , "pi", "π")
            ,(Just 'P' , "Pi", "Π")
            ,(Just 'r', "rho", "ρ")
            ,(Just 's', "sigma", "σ")
            ,(Just 'S', "Sigma", "Σ")
            ,(Just 't', "tau", "τ")
            ,(Just 'f' , "phi", "φ")
            ,(Just 'F' , "Phi", "Φ")
            ,(Just 'c', "chi", "χ")
            ,(Just 'C', "Chi", "Χ")
            ,(Nothing , "psi", "ψ")
            ,(Nothing , "Psi", "Ψ")
            ,(Just 'w', "omega", "ω")
            ,(Just 'O', "Omega", "Ω")
            ]


symbols :: [(String, String)]
symbols =
 [
 -- parens
  ("<","⟨")
 ,(">","⟩")
 ,("<>","⟨⟩")
 ,(">>","⟫")
 ,("<<","⟪")

-- These two confuse gnome-terminal.
 ,("|(","〖")
 ,(")|","〗")

 ,("{|", "⦃")
 ,("|}", "⦄")

 ,("[[","⟦")
 ,("]]","⟧")

 ,("|_","⌊")
 ,("_|","⌋")
 ,("|__|","⌊⌋")
 ,("r|_","⌈")
 ,("r_|","⌉")
 ,("r|__|","⌈⌉")


 ,("[]", "∎")

 -- quantifiers
 ,("forall", "∀")
 ,("all", "∀")
 ,("exists", "∃")
 ,("rA", "∀") -- reversed A
 ,("rE", "∃") -- reversed E
 ,("/rE", "∄")

 -- operators
 ,("<|","◃")
 -- ,("<|","◁") alternative
 ,("|>","▹")
 ,("><","⋈")
 ,("<)", "◅")
 ,("(>", "▻")
 ,("v","∨")
 ,("u","∪")
 ,("V","⋁")
 ,("+u","⊎")
 ,("u[]","⊔")
 ,("n[]","⊓")
 ,("^","∧")
 ,("/\\", "∧")
 ,("\\/", "∨")
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
 ,("<=>","⇔")
 ,("~>","↝")
 ,("<~","↜")
 ,("<-<", "↢")
 ,(">->", "↣")
 ,("<->", "↔")
 ,("|<-", "⇤")
 ,("->|", "⇥")
 ,(">>=","↠")

 --- relations
 ,("c=","⊆")
 ,("c","⊂")
 ,("c-","∈")
 ,("in","∈")
 ,("/c-","∉")
 ,("c/=","⊊")
 ,("rc=","⊇") -- r for reversed
 ,("rc","⊃") -- r for reversed
 ,("rc-","∋") -- r for reversed
 ,("r/c-","∌") -- r for reversed
 ,("rc/=","⊋") -- r for reversed
 ,(">=","≥")
 ,("=<","≤")
 ,("c[]","⊏")
 ,("rc[]","⊐")
 ,("c[]=","⊑")
 ,("rc[]=","⊒")
 ,("/c[]=","⋢")
 ,("/rc[]=","⋣")
 ,("c[]/=","⋤")
 ,("rc[]/=","⋥")

 ---- equal signs
 ,("=def","≝")
 ,("=?","≟")
 ,("==","≡")
 ,("~~","≈")
 ,("~-","≃")
 ,("~=","≅")
 ,("~","∼")
 ,("~~","≈")
 ,("/=","≠")
 ,("/==","≢")
 ,(":=","≔")
 ,("=:","≕")

 -- misc
 ,("_|_","⊥")
 ,("Top","⊤")
 ,("l","ℓ")
 ,("::","∷")
 ,(":", "∶")
 ,("0", "∅")
 ,("*", "★") -- or "⋆"
 ,("/'l","ƛ")
 ,("d","∂")
 ,("#b","♭") -- music bemol
 ,("#f","♮") -- music flat
 ,("##","♯") -- music #
 ,("Hot","♨")
 ,("Cut","✂")
 ,("Pen","✎")
 ,("Tick","✓")

 -- dashes
 ,("-","−")

 -- quotes
 ,("\"","“”")
 ,("r`","′")

 -- turnstyles
 ,("|-", "⊢")
 ,("|/-", "⊬")
 ,("-|", "⊣")
 ,("|=", "⊨")
 ,("|/=", "⊭")
 ,("||-", "⊩")

  -- circled/squared operators
  -- ⊝ ⍟ ⎊ ⎉

  ,("o+","⊕")
  ,("o-","⊖")
  ,("ox","⊗")
  ,("o/","⊘")
  ,("o*","⊛")
  ,("o=","⊜")
  ,("o.","⊙")
  ,("oo","⊚")
  ,("[+]","⊞")
  ,("[-]","⊟")
  ,("[x]","⊠")
  ,("[.]","⊡")
  ,("[]","∎")

 ] ++ [ (leading:l, [u]) | leading <- ['|','b'], (l,u) <-

 [("N",'ℕ')
 ,("H",'ℍ')
 ,("P",'ℙ')
 ,("R",'ℝ')
 ,("D",'ⅅ')
 ,("Q",'ℚ')
 ,("Z",'ℤ')
 ,("gg",'ℽ')
 ,("gG",'ℾ')
 ,("gP",'ℿ')
 ,("gS",'⅀')
 ]

 ] ++ [

  ("cP","℘") -- c for cal
 ,("cL","ℒ") -- c for cal
 ,("cR","ℛ") -- c for cal
 ]

checkAmbs :: [(String, String)] -> [(String, String)]
checkAmbs table = check
  where ambs = [ (x, y)
               | v@(x, _) <- table
               , w@(y, _) <- table
               , v /= w
               , x `isPrefixOf` y ]
        check | null ambs = table
              | otherwise = error $ "checkAmbs: ambiguous declarations for " ++ show ambs

disamb :: [(String, String)] -> [(String, String)]
disamb table = map f table
  where f v@(x, vx) =
            let ambs = [ w
                       | w@(y, _) <- table
                       , v /= w
                       , x `isPrefixOf` y ]
            in if null ambs then v else (x ++ " ", vx)


-- More:
-- arrows: ⇸ ⇆
-- circled operators: ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⍟  ⎊ ⎉
-- squared operators: ⊞ ⊟ ⊠ ⊡
-- turnstyles: ⊦ ⊧
-- subscript: ₔ

zipscripts :: Char -> String -> String -> [(String, String)]
zipscripts c ascii unicode
  = zip (fmap ((c:) . pure) ascii) (fmap pure unicode)

subscripts, superscripts :: [(String, String)]

subscripts   = zipscripts '_' "0123456789+-=()aeioruvx"
                              "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"

superscripts = zipscripts '^' -- NOTE that qCFQSVXYZ are missing
  "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
  "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"
