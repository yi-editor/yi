{-# LANGUAGE RankNTypes #-}

-- | Tests for pure manipulations of the editor in the Vim2 Keymap.
--
-- Pure manipulations of the editor refers to such things as changing layout,
-- navigating buffers, creating or deleting buffers, creating or deleting tabs.
-- In short, anything which 1) doesn't perform IO and 2) interacts with
-- something other than a single buffer.
--
-- If a test is pure and manipulates only a single buffer, it would be better
-- being part of the 'TestVim' module. That module provides a nicer way of
-- writing pure single buffer manipulation tests.
--
module TestVim2PureEditorManipulations (getTests) where

import Control.Monad (unless)

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Yi (extractValue)
import Yi.Buffer
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Keymap.Vim2


--------------------------------------------------
-- Common functions to move to a Common or Util library.

-- | Run the editor with the defaultVimConfig.
runEditor' :: EditorM a -> Editor -> (Editor, a)
runEditor' = runEditor defaultVimConfig


-- | Run a pure editor manipulation test.
--
-- Runs the @setupActions@ against an empty editor. Checks that @preConditions@
-- hold for that editor. Then runs @testActions@ against the setup editor.
-- Finally checks that @assertions@ hold for the final editor state.
--
-- @preConditions@, @testActions@ and @assertions@ are each passed the return
-- value of @setupActions@.
-- 
runTest :: EditorM a
        -- ^ Setup actions to initialize the editor.
        -> (Editor -> a -> Assertion)
        -- ^ Precondition assertions. Used to check that the editor is in the
        -- expected state prior to running the test actions.
        -> (a -> EditorM ())
        -- ^ The actions to run as part of the test. The return value from
        -- the setup action is passed to this.
        -> (Editor -> a -> Assertion)
        -- ^ Assertions to check that the editor is in the expected state.
        -- The return value from the setup action is passed to this.
        -> Assertion
runTest setupActions preConditions testActions assertions = do
    let (setupEditor, a) = runEditor' setupActions emptyEditor
    preConditions setupEditor a
    let finalEditor = fst $ runEditor' (testActions a) setupEditor
    assertions finalEditor a


defVimEval :: String -> EditorM ()
defVimEval = pureEval (extractValue defVimConfig)


extractBufferString :: Editor -> String
extractBufferString editor = snd (runEditor' (withBuffer0 elemsB) editor)


assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
    unless (actual /= expected) (assertFailure msg)
  where
    msg = (if null preface then "" else preface ++ "\n") ++
          "expected not to get: " ++ show expected

--------------------------------------------------
-- Buffer tests to move to a Buffer test module.


-- | Create three buffers and return the 'BufferRef' of the second buffer created.
-- Which should be unfoccused.
createInitialBuffers :: EditorM (BufferRef, String)
createInitialBuffers = do
    one   <- newBufferE (Right "one")   (fromString "Buffer one")
    two   <- newBufferE (Right "two")   (fromString "Buffer two")
    three <- newBufferE (Right "three") (fromString "Buffer three")
    return $! (two, "two")

assertContentOfCurrentBuffer :: String -> Editor -> Assertion
assertContentOfCurrentBuffer content editor = 
    assertEqual "Wrong buffer content" content (extractBufferString editor)

assertNotCurrentBuffer :: BufferRef -> Editor -> Assertion
assertNotCurrentBuffer bufref editor =
    assertNotEqual "Unexpected current buffer" bufref (currentBuffer editor)

assertCurrentBuffer :: BufferRef -> Editor -> Assertion
assertCurrentBuffer bufref editor =
    assertEqual "Unexpected current buffer" bufref (currentBuffer editor)


--------------------------------------------------
-- Driver to stay here and import other modules
getTests :: TestTree
getTests = 
    testGroup "Vim2 pure editor manipulation tests" [
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
    ]
