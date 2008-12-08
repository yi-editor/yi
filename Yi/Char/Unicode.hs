module Yi.Char.Unicode (greek, symbols) where
greek :: [(String, String)]
greek = [("alpha", "α")
        ,("beta", "β")
        ,("gamma", "γ")
        ,("Gamma", "Γ")
        ,("delta", "δ")
        ,("Delta", "Δ")
        ,("epsilon", "ε")
        ,("zeta", "ζ")
        ,("eta", "η")
        ,("theta", "θ")
        ,("Theta", "Θ")
        ,("iota", "ι")
        ,("kapa", "κ")
        ,("lambda", "λ")
        ,("Lambda", "Λ")
        ,("mu", "μ")
        ,("nu", "ν")
        ,("xi", "ξ")
        ,("omicron", "ο")
        ,("pi", "π")
        ,("Pi", "Π")
        ,("rho", "ρ")
        ,("sigma", "σ")
        ,("Sigma", "Σ")
        ,("tau", "τ")
        ,("phi", "φ")
        ,("Phi", "Φ")
        ,("chi", "χ")
        ,("Chi", "Χ")
        ,("psi", "ψ")
        ,("Psi", "Ψ")
        ,("omega", "ω")
        ,("Omega", "Ω")
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
 ,("|R","ℝ")
 ,("^n","ⁿ")
 ,("::","∷")

 -- dashes
 ,("-","−")

 -- quotes
 ,("\"","“”")
 ]

-- More:
-- arrows: ↢ ↣   ↔  ⇤  ⇥  ⇸ ⇆
-- set: ∅ ∉ ∈ ⊇ ⊃
-- relations: ≝ ≤ ≥
-- circled operators: ⊕ ⊖ ⊗ ⊘ ⊙ ⊚ ⊛ ⊜ ⊝ ⍟  ⎊ ⎉
-- squared operators: ⊞ ⊟ ⊠ ⊡
-- turnstyles: ⊢ ⊣ ⊤ ⊥ ⊦ ⊧ ⊨ ⊩ ⊬ ⊭
