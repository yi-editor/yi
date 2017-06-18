{-# LANGUAGE OverloadedStrings #-}
module Yi.Mode.CommonTests (testSuite) where

import Test.Tasty
import Test.Tasty.HUnit

import Yi.Mode.Common
import Data.Attoparsec.Text (parseOnly)
import Control.Applicative ((<|>))
import Data.Either (isRight)

testSuite :: TestTree
testSuite = testGroup "Mode.Common" [unitTests]

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testGroup "shebangParser" $
      [ testCase "matches a simple shebang" $
          parseOnly (shebangParser "runhaskell") "#!/usr/bin/env runhaskell\n" @?= Right ()
      , testCase "matches a complex shebang" $
          map (parseOnly (shebangParser ("python" *> ("3" <|> "2" <|> "")))) ["#!/usr/bin/env python\n", "#!/usr/bin/env python2\n", "#!/usr/bin/env python3\n"] @?= [Right (), Right (), Right ()]
      , testCase "ignores noise and spaces" $
          parseOnly (shebangParser "runhaskell") "\n#!abcdefg\r\nABCdefG\n#!   /usr/bin/env  runhaskell    \r\n\n/AbcDe#!/fg\n" @?= Right ()
      , testCase "parser fails correctly" $
          isRight (parseOnly (shebangParser "runhaskell") "#!/usr/bin/env abc\n") @?= False
      ]
  ]
