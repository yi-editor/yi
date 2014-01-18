-- | Tests for the :buffer ex command in the Vim2 keymap
--
module Vim2.EditorManipulations.BufferExCommand (tests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Yi.Buffer
import Yi.Editor

import Vim2.TestUtils


-- | Create three buffers and return the 'BufferRef' of the second buffer created.
-- Which should be unfoccused.
createInitialBuffers :: EditorM (BufferRef, String)
createInitialBuffers = do
    one   <- newBufferE (Right "one")   (fromString "Buffer one")
    two   <- newBufferE (Right "two")   (fromString "Buffer two")
    three <- newBufferE (Right "three") (fromString "Buffer three")
    return $! (two, "two")


tests :: TestTree
tests =
    testGroup ":buffer" [
        testCase "Switches to the named buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor (bufferKey, _name) = do
                    assertNotCurrentBuffer bufferKey editor

                testActions (_, name) =
                    defVimEval $ ":buffer " ++ name ++ "<CR>"

                assertions editor (bufferKey, _) = do
                    assertContentOfCurrentBuffer "Buffer two" editor
                    assertCurrentBuffer bufferKey editor

            runTest setupActions preConditions testActions assertions


      , testCase "Switches to the numbered buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor (bufferKey, _) = do
                    assertNotCurrentBuffer bufferKey editor

                testActions (BufferRef bref, _) =
                    defVimEval $ ":buffer " ++ show bref ++ "<CR>"

                assertions editor (bufferKey, _) = do
                    assertContentOfCurrentBuffer "Buffer two" editor
                    assertCurrentBuffer bufferKey editor

            runTest setupActions preConditions testActions assertions
    ]