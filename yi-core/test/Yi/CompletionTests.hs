module Yi.CompletionTests (testSuite) where

import Data.List (sort,nub)
import Data.Maybe(isJust)
import Data.Monoid
import Data.Text.Arbitrary()
import Test.Tasty
import Test.Tasty.QuickCheck
import Yi.Completion as C
import qualified Data.Map as M
import qualified Data.Text as T

testSuite :: TestTree
testSuite = testGroup "Completion" [propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "properties"
  [ testProperty "infixUptoEndMatch needle (pre <> needle <> post) == Just (needle <> post) if needle and post not empty and needle not in pre" $
      \pre needle post ->
           not (needle `T.isInfixOf` pre) ==>
           not (T.null post) ==>
             infixUptoEndMatch needle (pre <> needle <> post) == Just (needle <> post)
  , testProperty "infixUptoEndMatch \"\" x == Just x" $
      \x -> infixUptoEndMatch T.empty x == Just x
  , testProperty "isJust (infixUptoEndMatch needle haystack) == needle `Data.Text.isInfixOf` haystack" $
      \needle haystack ->
            isJust (infixUptoEndMatch needle haystack) == needle `T.isInfixOf` haystack
  ]
