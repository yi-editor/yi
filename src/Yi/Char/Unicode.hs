module Yi.Char.Unicode (greek, symbols, subscripts, superscripts, checkAmbs, disamb) where

import Data.List (isPrefixOf)

greek :: [(String, String)]
greek = [(name, unicode) | (_,name,unicode) <- greekData] ++ 
        [(['\'',shorthand],unicode) | (Just shorthand,_,unicode) <- greekData]

-- | Triples: (shorthand, name, unicode)
greekData :: [(Maybe Char, String, String)]
greekData = [(Just 'a', "alpha", "α")
            ,(Just 'b', "beta", "β")
            ,(Just 'g', "gamma", "γ")
            ,(Just 'G', "Gamma", "Γ")
            ,(Just 'd', "delta", "δ")
            ,(Just 'D', "Delta", "Δ")
            ,(Nothing , "epsilon", "ε")
            ,(Just 'z', "zeta", "ζ")
            ,(Nothing , "eta", "η")
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
            ,(Nothing , "pi", "π")
            ,(Nothing , "Pi", "Π")
            ,(Just 'r', "rho", "ρ")
            ,(Just 's', "sigma", "σ")
            ,(Just 'S', "Sigma", "Σ")
            ,(Just 't', "tau", "τ")
            ,(Nothing , "phi", "φ")
            ,(Nothing , "Phi", "Φ")
            ,(Nothing , "chi", "χ")
            ,(Nothing , "Chi", "Χ")
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
 ,(">>","⟫")
 ,("<<","⟪")

-- These two confuse gnome-terminal.
 ,("|(","〖")
 ,(")|","〗")

 ,("{|", "⦃")
 ,("|}", "⦄")

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
 ,("+u","⊎")
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
 ,("<=>","⇔")
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
 ,("==","≡")
 ,("~-","≃")
 ,("~=","≅")
 ,("~","∼")
 ,("~~","≈")
 ,("/=","≠")
 ,("/==","≢")

 -- misc
 ,("_|_","⊥")
 ,("Top","⊤")
 ,("|N","ℕ")
 ,("|P","ℙ")
 ,("|R","ℝ")
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
-- set:  ⊇ ⊃
-- circled operators: ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⍟  ⎊ ⎉
-- squared operators: ⊞ ⊟ ⊠ ⊡
-- turnstyles: ⊦ ⊧


subscripts, superscripts :: [(String, String)]
subscripts   = zip (fmap (('_':). (:[])) "0123456789+-=()")  (fmap (:[]) "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎")
superscripts = zip (fmap (('^':). (:[])) "0123456789+-=()n") (fmap (:[]) "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿ")
