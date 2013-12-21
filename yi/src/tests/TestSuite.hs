module Main where

import Test.Framework (defaultMain)

import Test.Framework.Providers.HUnit

import qualified TestVim

main :: IO ()
main = do
    tests <- TestVim.getTests
    defaultMain tests
