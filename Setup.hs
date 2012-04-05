#!/usr/bin/env runhaskell

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks

