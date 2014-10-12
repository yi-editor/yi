{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the :buffer ex command in the Vim keymap
--
module Vim.EditorManipulations.BufferExCommand (tests) where

import qualified Data.List.NonEmpty as NE
import           Generic.TestUtils
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Yi.Buffer
import           Yi.Config (Config)
import           Yi.Editor
import           Yi.Rope ()

type BufferName = String

-- | Create three buffers and return the 'BufferRef' and buffer name of
-- each.
createInitialBuffers :: EditorM [(BufferRef, BufferName)]
createInitialBuffers = do
  one   <- newBufferE (FileBuffer "one")   "Buffer one"
  two   <- newBufferE (FileBuffer "two")   "Buffer two"
  three <- newBufferE (FileBuffer "three") "Buffer three"
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


      , testCase ":buffer # switches to the previous buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertEqual "Unexpected buffer stack"
                        [nthBufferRef 2 buffers, nthBufferRef 1 buffers]
                        (take 2 . NE.toList $ bufferStack editor)

                testActions _ =
                    ev $ ":buffer #<CR>"

                assertions editor buffers = do
                    assertEqual "Unexpected buffer stack"
                        [nthBufferRef 1 buffers, nthBufferRef 2 buffers]
                        (take 2 . NE.toList $ bufferStack editor)

            runTest setupActions preConditions testActions assertions c


      , testCase ":buffer % is a no-op" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertCurrentBuffer (nthBufferRef 2 buffers) editor

                testActions _ =
                    ev $ ":buffer %<CR>"

                assertions editor buffers = do
                    assertContentOfCurrentBuffer c "Buffer three" editor
                    assertCurrentBuffer (nthBufferRef 2 buffers) editor

            runTest setupActions preConditions testActions assertions c


      , testCase ":buffer is a no-op" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertCurrentBuffer (nthBufferRef 2 buffers) editor

                testActions _ =
                    ev $ ":buffer<CR>"

                assertions editor buffers = do
                    assertContentOfCurrentBuffer c "Buffer three" editor
                    assertCurrentBuffer (nthBufferRef 2 buffers) editor

            runTest setupActions preConditions testActions assertions c


      , testCase "A modified buffer is not abandoned" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

                testActions buffers = do
                    withCurrentBuffer $ insertN "The buffer is altered"
                    ev $ ":buffer " ++ nthBufferName 1 buffers ++ "<CR>"

                assertions editor buffers = do
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

            runTest setupActions preConditions testActions assertions c


      , testCase "A modified buffer can be abandoned with a bang" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

                testActions buffers = do
                    withCurrentBuffer $ insertN "The buffer is altered"
                    ev $ ":buffer! " ++ nthBufferName 1 buffers ++ "<CR>"

                assertions editor buffers = do
                    assertCurrentBuffer (nthBufferRef 1 buffers) editor

            runTest setupActions preConditions testActions assertions c


      , testCase ":Nbuffer switches to the numbered buffer" $ do
            let setupActions = createInitialBuffers

                preConditions editor buffers =
                    assertNotCurrentBuffer (nthBufferRef 1 buffers) editor

                testActions buffers =
                    -- return ()
                    let (BufferRef bref) = nthBufferRef 1 buffers
                    in ev $ ":" ++ show bref ++ "buffer<CR>"
                    -- in ev $ ":buffer " ++ show bref ++ "<CR>"

                assertions editor buffers = do
                    -- assertContentOfCurrentBuffer c "Buffer two" editor
                    assertCurrentBuffer (nthBufferRef 1 buffers) editor

            runTest setupActions preConditions testActions assertions c


      -- , testCase "A named buffer can be shown in a split window" $ do
      -- , testCase "A numbered buffer can be shown in a split window" $ do
    ]
