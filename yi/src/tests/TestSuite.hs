module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TestVim
import qualified TestExParser
import qualified TestVim2PureEditorManipulations

main :: IO ()
main = do
    tests  <- TestVim.getTests
    defaultMain $ testGroup "Tests" [ 
        tests
      , TestExParser.getTests
      , TestVim2PureEditorManipulations.getTests
      ]
