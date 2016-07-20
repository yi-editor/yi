import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Yi.Regex
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Text.Regex.TDFA.Pattern

ignoreDoPa :: Pattern -> Pattern
ignoreDoPa (PCarat  dp   ) = PCarat  (DoPa 0)
ignoreDoPa (PDollar dp   ) = PDollar (DoPa 0)
ignoreDoPa (PDot    dp   ) = PDot    (DoPa 0)
ignoreDoPa (PAny    dp ps) = PAny    (DoPa 0) ps
ignoreDoPa (PAnyNot dp ps) = PAnyNot (DoPa 0) ps
ignoreDoPa (PEscape dp pc) = PEscape (DoPa 0) pc
ignoreDoPa (PChar   dp pc) = PChar   (DoPa 0) pc
ignoreDoPa (PGroup  m p  ) = PGroup  m   (ignoreDoPa p)
ignoreDoPa (POr     l    ) = POr         (map ignoreDoPa l)
ignoreDoPa (PConcat l    ) = PConcat     (map ignoreDoPa l)
ignoreDoPa (PQuest  p    ) = PQuest      (ignoreDoPa p)
ignoreDoPa (PPlus   p    ) = PPlus       (ignoreDoPa p)
ignoreDoPa (PStar   b p  ) = PStar   b   (ignoreDoPa p)
ignoreDoPa (PBound  i m p) = PBound  i m (ignoreDoPa p)
ignoreDoPa (PNonCapture p) = PNonCapture (ignoreDoPa p)
ignoreDoPa (PNonEmpty   p) = PNonEmpty   (ignoreDoPa p)
ignoreDoPa p = p

mapFst f (a,b) = (f a,b)

main = defaultMain =<< tests

tests = testSpec "(Hspec tests)" $ do
  describe "reversePattern" $ do
    it "reverses normal characters" $
      (mapFst ignoreDoPa . reversePattern <$> parseRegex "ab") 
        `shouldBe` (mapFst ignoreDoPa <$> parseRegex "ba")

    it "changes carat to dollar" $
      (reversePattern <$> parseRegex "^") `shouldBe` parseRegex "$"

    it "changes dollar to carat" $
      (reversePattern <$> parseRegex "$") `shouldBe` parseRegex "^"

    it "forms the identity when applied twice" $
      property $ \p -> (reversePattern . reversePattern <$> parseRegex p) `shouldBe` parseRegex p

    it "recursively reverses patterns" $
      (mapFst ignoreDoPa . reversePattern <$> parseRegex "foo|bar") 
        `shouldBe` (mapFst ignoreDoPa <$> parseRegex "oof|rab")