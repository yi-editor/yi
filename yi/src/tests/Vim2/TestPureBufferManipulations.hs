{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for pure manipulations of a single buffer in the Vim2 Keymap.
--
-- A manipulation of a single buffer is an operation or sequence of operations
-- which do nothing other than change the contents or cursor position of a
-- single buffer.
--
-- This module loads the tests from files in @src/tests/vimtests@. Adding new
-- tests, or altering existing tests is done by editing files there. The format
-- should be self explanatory.
--
-- If a test is pure and manipulates something other than the contents or cursor
-- position of a single buffer, it should be added to the
-- 'Vim2.TestPureEditorManipulations' module.
--
module Vim2.TestPureBufferManipulations (getTests) where

import qualified Generic.TestPureBufferManipulations as GT
import           Test.Tasty (TestTree)
import           Yi (extractValue)
import           Yi.Config.Default (defaultVimConfig)
import           Yi.Keymap.Vim2

getTests :: IO TestTree
getTests = GT.getTests defaultVimConfig "src/tests/vimtests"
           "Vim2" (pureEval $ extractValue defVimConfig)
