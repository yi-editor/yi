module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TestVim
import qualified TestExParser

main :: IO ()
main = do
    tests  <- TestVim.getTests
    defaultMain $ testGroup "Tests" [ 
        tests
      , TestExParser.getTests
      ]
