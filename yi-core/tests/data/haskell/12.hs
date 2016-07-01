module Yi.Char.Unicode (greek, symbols, subscripts) where
greek :: [(String, String)]
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


subscripts :: [(String, String)]
subscripts = zip (fmap (('_':). (:[])) "0123456789+-=()") (fmap (:[]) "₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎")
