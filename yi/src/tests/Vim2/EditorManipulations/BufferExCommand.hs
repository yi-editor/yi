-- | Tests for the :buffer ex command in the Vim2 keymap
--
module Vim2.EditorManipulations.BufferExCommand (tests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Yi.Buffer
import Yi.Editor
import Yi.Config (Config)

import Generic.TestUtils

type BufferName = String

-- | Create three buffers and return the 'BufferRef' and buffer name of
-- each.
createInitialBuffers :: EditorM [(BufferRef, BufferName)]
createInitialBuffers = do
    one   <- newBufferE (Right "one")   (fromString "Buffer one")
    two   <- newBufferE (Right "two")   (fromString "Buffer two")
    three <- newBufferE (Right "three") (fromString "Buffer three")
    return [(one, "one"), (two, "two"), (three, "three")]


nthBufferRef :: Int -> [(BufferRef, BufferName)] -> BufferRef
nthBufferRef n buffers = fst $ buffers !! n

nthBufferName :: Int -> [(BufferRef, BufferName)] -> BufferName
nthBufferName n buffers = snd $ buffers !! n


tests :: Config -> KeyEval -> TestTree
tests c ev =
    testGroup ":buffer" [
        testCase ":buffer {bufname} switches to the named buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

                testActions buffers =
                    ev $ ":buffer " ++ nthBufferName 1 buffers ++ "<CR>"

                assertions editor buffers = do
                    assertContentOfCurrentBuffer c "Buffer two" editor
                    assertCurrentBuffer (nthBufferRef 1 buffers) editor

            runTest setupActions preConditions testActions assertions c


      , testCase ":buffer N switches to the numbered buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

                testActions buffers =
                    let (BufferRef bref) = nthBufferRef 1 buffers
                    in ev $ ":buffer " ++ show bref ++ "<CR>"

                assertions editor buffers = do
                    assertContentOfCurrentBuffer c "Buffer two" editor
                    assertCurrentBuffer (nthBufferRef 1 buffers) editor

            runTest setupActions preConditions testActions assertions c
    ]
