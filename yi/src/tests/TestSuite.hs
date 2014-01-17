module Main where

import Test.Framework (defaultMain)

import Test.Framework.Providers.HUnit

import qualified TestVim
import qualified TestExParser

main :: IO ()
main = do
    tests  <- TestVim.getTestGroup
    defaultMain $ [tests, TestExParser.getTestGroup]
