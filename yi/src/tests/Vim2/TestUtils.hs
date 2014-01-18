module Vim2.TestUtils where

import Control.Monad (unless)

import Test.Tasty.HUnit

import Yi (extractValue)
import Yi.Buffer
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Keymap.Vim2

-- | Run the editor with the defaultVimConfig.
runEditor' :: EditorM a -> Editor -> (Editor, a)
runEditor' = runEditor defaultVimConfig


-- | Evaluate a string against the default Vim configuration.
--
-- The string is interpreted as a sequence of keystrokes starting in Normal mode.
-- To evaluate the `:buffer` ex command, for instance, use the string
-- `:buffer<CR>`.
--
-- Only pure commands will be run.
--
defVimEval :: String -> EditorM ()
defVimEval = pureEval (extractValue defVimConfig)

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


-- Return the contents of the current buffer as a string.
extractBufferString :: Editor -> String
extractBufferString editor =
    snd (runEditor' (withBuffer0 elemsB) editor)


--------------------------------------------------
-- Useful assertions.

-- | Asserts that the specified actual value is not equal to the unexpected
-- value. The output message will contain the prefix and the actual value.
--  
-- If the prefix is the empty string (i.e., @\"\"@), then the prefix is omitted
-- and only the actual value is output.
assertNotEqual :: (Eq a, Show a) => String  -- ^ The message prefix
                                 -> a       -- ^ The expected value
                                 -> a       -- ^ The actual value
                                 -> Assertion
assertNotEqual preface expected actual =
    unless (actual /= expected) (assertFailure msg)
  where
    msg = (if null preface then "" else preface ++ "\n") ++
          "expected not to get: " ++ show expected


-- | Asserts that the contents of the current buffer are equal to the expected
-- value. The output message will contain the expected value and the actual value.
assertContentOfCurrentBuffer :: String -> Editor -> Assertion
assertContentOfCurrentBuffer expectedContent editor = 
    assertEqual "Unexpected buffer content" expectedContent (extractBufferString editor)


-- | Asserts that the current buffer is not the specified buffer. The output will
-- contain the BufferKey of the current buffer.
assertNotCurrentBuffer :: BufferRef -> Editor -> Assertion
assertNotCurrentBuffer bufref editor =
    assertNotEqual "Unexpected current buffer" bufref (currentBuffer editor)


-- | Asserts that the current buffer is the expected buffer. The output will
-- contain the expected BufferKey and the acutal BufferKey of the current buffer.
assertCurrentBuffer :: BufferRef -> Editor -> Assertion
assertCurrentBuffer bufref editor =
    assertEqual "Unexpected current buffer" bufref (currentBuffer editor)