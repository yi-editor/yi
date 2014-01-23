-- | Tests for the :buffer ex command in the Vim2 keymap
--
module Vim2.EditorManipulations.BufferExCommand (tests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Yi.Buffer
import Yi.Editor
import Yi.Config (Config)

import Generic.TestUtils


-- | Create three buffers and return the 'BufferRef' of the second buffer
-- created which should be unfoccused.
createInitialBuffers :: EditorM (BufferRef, String)
createInitialBuffers = do
    _   <- newBufferE (Right "one")   (fromString "Buffer one")
    two   <- newBufferE (Right "two")   (fromString "Buffer two")
    _ <- newBufferE (Right "three") (fromString "Buffer three")
    return (two, "two")

tests :: Config -> KeyEval -> TestTree
tests c ev =
    testGroup ":buffer" [
        testCase "Switches to the named buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor (bufferKey, _name) =
                    assertNotCurrentBuffer bufferKey editor

                testActions (_, name) =
                    ev $ ":buffer " ++ name ++ "<CR>"

                assertions editor (bufferKey, _) = do
                    assertContentOfCurrentBuffer c "Buffer two" editor
                    assertCurrentBuffer bufferKey editor

            runTest setupActions preConditions testActions assertions c


      , testCase "Switches to the numbered buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor (bufferKey, _) =
                    assertNotCurrentBuffer bufferKey editor

                testActions (BufferRef bref, _) =
                    ev $ ":buffer " ++ show bref ++ "<CR>"

                assertions editor (bufferKey, _) = do
                    assertContentOfCurrentBuffer c "Buffer two" editor
                    assertCurrentBuffer bufferKey editor

            runTest setupActions preConditions testActions assertions c
    ]
