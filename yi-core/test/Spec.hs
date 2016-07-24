import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Yi.CompletionTree as CT
import           Data.List         (nub, sort)
import qualified Data.Map          as M

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [completionTreeTests]

completionTreeTests :: TestTree
completionTreeTests = testGroup "CompletionTree"
    [ completionTreePropertyTests
    , completionTreeUnitTests
    ]

completionTreePropertyTests :: TestTree
completionTreePropertyTests = testGroup "properties"
  [ testProperty "CT.toList . CT.fromList == nub . sort" $
      \list -> CT.toList (CT.fromList list) == sort (nub (list :: [String]))
  , testProperty "update (fromList [a]) a == fromList [mempty] (a is a non-empty string)" $
      \string -> null string || CT.update (CT.fromList [string :: String]) string == CT.fromList [mempty]
  , testProperty "\"\" `elem` update (fromList [a,...]) a" $
      \listOfStrings -> null listOfStrings || null (head listOfStrings) || "" `elem` CT.toList (CT.update (CT.fromList listOfStrings) (head listOfStrings))
  , testProperty "complete (fromList [a]) == (a, fromList [\"\"])" $
      \string -> CT.complete (CT.fromList [string]) == (string,CT.fromList [""])
  ]

completionTreeUnitTests :: TestTree
completionTreeUnitTests = testGroup "unit tests"
  [ testGroup "fromList"
      [ testCase "returns an empty CompletionTree when given an empty list" $
          CT.fromList [] @?= (mempty :: CT.CompletionTree String)
      , testCase "returns a map with one key when given a list with one item" $
          CT.fromList ["a"] @?= CT.CompletionTree (M.fromList [("a",mempty)])
      , testCase "groups elements with the same prefix" $
          CT.fromList ["aa","ab"] @?= CT.CompletionTree (M.fromList [("a",CT.CompletionTree $ M.fromList [("a",mempty),("b",mempty)])])
      ]
  , testGroup "update"
    -- toList is covered by the SmallCheck and QuickCheck
      [ testCase "strips its argument from a matching key" $
          CT.update (CT.fromList ["abc"]) "a" @?= CT.fromList ["bc"]
      , testCase "descends the tree if a substring of its input is found in the CompletionTree" $
          CT.update (CT.fromList ["put","putStr"]) "putS" @?= CT.fromList ["tr"]
      , testCase "returns an empty list if it can't find a matching key" $
          CT.update (CT.fromList ["put"]) "list" @?= CT.fromList []
      ]
  , testGroup "complete"
      [ testCase "Returns the common prefix" $
        CT.complete (CT.fromList ["put","putStr","putStrLn"]) @?= ("put",CT.fromList ["","Str","StrLn"])
      , testCase "Returns an empty string if there's no common prefix" $
        CT.complete (CT.fromList ["put","putStr","abc"]) @?= ("",CT.fromList ["put","putStr","abc"])
      ]
  ]
