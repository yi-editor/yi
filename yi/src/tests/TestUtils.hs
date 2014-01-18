module TestUtils where

import Control.Monad (unless)

import Test.Tasty.HUnit

import Yi.Buffer
import Yi.Editor

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
        -- ^ Precondition assertions. Used to check that the editor
        -- is in the expected state prior to running the test actions.
        -> (a -> EditorM ())
        -- ^ The actions to run as part of the test. The return value
        -- from the setup action is passed to this.
        -> (Editor -> a -> Assertion)
        -- ^ Assertions to check that the editor is in the expected
        -- state. The return value from the setup action is passed to
        -- this.
        -> Assertion
runTest setupActions preConditions testActions assertions = do
    let (setupEditor, a) = runEditor' setupActions emptyEditor
    preConditions setupEditor a
    let finalEditor = fst $ runEditor' (testActions a) setupEditor
    assertions finalEditor a


extractBufferString :: Editor -> String
extractBufferString editor =
    snd (runEditor' (withBuffer0 elemsB) editor)


assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
    unless (actual /= expected) (assertFailure msg)
  where
    msg = (if null preface then "" else preface ++ "\n") ++
          "expected not to get: " ++ show expected

assertContentOfCurrentBuffer :: String -> Editor -> Assertion
assertContentOfCurrentBuffer content editor = 
    assertEqual "Unexpected buffer content" content (extractBufferString editor)

assertNotCurrentBuffer :: BufferRef -> Editor -> Assertion
assertNotCurrentBuffer bufref editor =
    assertNotEqual "Unexpected current buffer" bufref (currentBuffer editor)

assertCurrentBuffer :: BufferRef -> Editor -> Assertion
assertCurrentBuffer bufref editor =
    assertEqual "Unexpected current buffer" bufref (currentBuffer editor)
