import Yi.Yi

instance Arbitrary FBuffer where
    arbitrary = do b0 <- newB 0 "*buffername*" =<< arbitrary
                   p0 <- arbitrary
                   return $ snd $ runBufferDummyWindow b0 (moveTo p0)


prop_replace_point b = snd $ runBufferDummyWindow b $ do
  p0 <- pointB
  replaceRegionB r
  p1 <- pointB
  return $ (p1 - p0) == ...