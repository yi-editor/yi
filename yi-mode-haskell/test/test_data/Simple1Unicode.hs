{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module Simple1 where

foo ∷ ∀ a. Eq (a ∷ ★) ⇒ a → IO ()
foo _ = do
  i ← return ()
  return i

bar x = proc x → id ⤙ x + 1
baz x = proc x → id ⤛ x + 1
bar' x = proc x → x + 1 ⤚ id
baz' x = proc x → x + 1 ⤜ id

main ∷ IO ()
main = putStrLn "Hello World!"
