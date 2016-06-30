{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for pure manipulations of a single buffer in the Vim Keymap.
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
-- 'Vim.TestPureEditorManipulations' module.
--
module Vim.TestPureBufferManipulations (getTests) where

import qualified Data.Text as T
import qualified Generic.TestPureBufferManipulations as GT
import           Test.Tasty (TestTree)
import           Yi (extractValue)
import           Yi.Config.Default (defaultVimConfig)
import           Yi.Keymap.Vim
import           Yi.Keymap.Vim.Common

getTests :: IO TestTree
getTests = GT.getTests defaultVimConfig "src/tests/vimtests"
           "Vim" (pureEval (extractValue defVimConfig) . Ev . T.pack)
