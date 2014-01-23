-- | Tests for pure manipulations of the editor in the Vim2 Keymap.
--
-- Pure manipulations of the editor refers to such things as changing layout,
-- navigating buffers, creating or deleting buffers, creating or deleting tabs.
-- In short, anything which 1) doesn't perform IO and 2) interacts with
-- something other than a single buffer.
--
-- If a test is pure and manipulates only a single buffer, it would be better
-- being part of the 'Vim2.TestPureBufferManipulations' module. That module
-- provides a nicer way of writing pure single buffer manipulation tests.
--
module Vim2.TestPureEditorManipulations (tests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Yi.Buffer
import Yi.Editor

import Vim2.TestUtils
import qualified Vim2.EditorManipulations.BufferExCommand as BufferExCommand


tests :: TestTree
tests = 
    testGroup "Vim2 pure editor manipulation tests"
        [ BufferExCommand.tests
        ]
