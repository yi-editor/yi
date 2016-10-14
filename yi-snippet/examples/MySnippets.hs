{-# LANGUAGE OverloadedStrings #-}

module MySnippets
    ( mySnippets
    ) where

import Control.Applicative
import Data.Char (isUpper)
import Data.Monoid
import qualified Data.Text as T
import System.FilePath

import qualified Yi.Rope as R
import Snippet

mySnippets :: [Snippet]
mySnippets =
    [ Snippet "m" $ do
        moduleName <- guessModuleName <$> refer filename
        line ("module " <> moduleName)
        lit "    (" >> finish >> nl
        lit "    ) where"
    , Snippet "lp" $ do
        lit "{-# LANGUAGE "
        _ <- place "OverloadedStrings"
        lit " #-}"
    , Snippet "iq" $ do
        lit "import qualified "
        moduleName <- place "Data.Map.Strict"
        lit " as "
        abbrev <- R.filter (`elem` ['A'..'Z']) . dropCommon <$> refer moduleName
        lit abbrev
    , Snippet "main" $ do
        line "def main():\n    "
        finish
        nl
        nl
        line "if __name__ == '__main__':"
        line "    main()"
    , Snippet "testsuite" $ do
        lit "Test-Suite " >> place "TestMain" >> nl
        line "  type: exitcode-stdio-1.0"
        lit "  main-is: " >> place "TestMain.hs" >> nl
        line "  hs-source-dirs: test"
        line "  ghc-options: -Wall -ferror-spans"
        line "  default-language:    Haskell2010"
        line "  build-depends:"
        line "    base >= 4.8"
    , Snippet "testmain" $ do
        line "{-# LANGUAGE TemplateHaskell #-}"
        line ""
        line "import Test.Tasty.TH"
        line "import Test.Tasty.HUnit"
        line ""
        line "main :: IO ()"
        line "main = $defaultMainGenerator"
        line ""
        line "case_trivial :: Assertion"
        line "case_trivial = True @?= True"
    , Snippet "cl" $ do
        className <-
            lit "@interface " *> place "ShinyClass" <* lit ": NSObject" <* nl
        line "@end"
        nl
        lit "@implementation " >> mirror className >> nl
        line "@end"
    ]

guessModuleName :: R.YiString -> R.YiString
guessModuleName =
    R.fromText . T.intercalate "."
        . reverse . takeWhile isCapitalized . reverse
        . T.splitOn "/"
        . T.pack . dropExtension . T.unpack
        . R.toText
    where
    isCapitalized s = case T.uncons s of
        Just (c, _) -> isUpper c
        Nothing -> False

dropCommon :: R.YiString -> R.YiString
dropCommon s =
    case (R.split (== '.') s) of
        [x] -> x
        "Control" : rest -> R.intercalate "." rest
        "Data" : rest -> R.intercalate "." rest
