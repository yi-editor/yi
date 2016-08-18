{-# LANGUAGE OverloadedStrings #-}
-- | Tests for pure manipulations of the editor in the Vim Keymap.
--
-- Pure manipulations of the editor refers to such things as changing layout,
-- navigating buffers, creating or deleting buffers, creating or deleting tabs.
-- In short, anything which 1) doesn't perform IO and 2) interacts with
-- something other than a single buffer.
--
-- If a test is pure and manipulates only a single buffer, it would be better
-- being part of the 'Vim.TestPureBufferManipulations' module. That module
-- provides a nicer way of writing pure single buffer manipulation tests.
--
module Vim.TestPureEditorManipulations (tests) where

import qualified Data.Text as T
import           Test.Tasty (TestTree, testGroup)
import qualified Vim.EditorManipulations.BufferExCommand as BufferExCommand
import           Yi (extractValue)
import           Yi.Config.Default (defaultConfig)
import           Yi.Keymap.Vim
import           Yi.Keymap.Vim.Common
import           Yi.Types (Config (..))

tests :: TestTree
tests =
    testGroup "Vim pure editor manipulation tests"
        [ BufferExCommand.tests yiConfig
            (pureEval (extractValue defVimConfig) . Ev . T.pack)
        ]
    where
    yiConfig = defaultConfig {defaultKm = keymapSet}
