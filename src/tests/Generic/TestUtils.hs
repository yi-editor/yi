module Generic.TestUtils where

import           Control.Monad (unless)
import           Test.Tasty.HUnit
import           Yi.Buffer
import           Yi.Config (Config)
import           Yi.Editor
import qualified Yi.Rope as R


type KeyEval = String -> EditorM ()
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
        -> Config
        -- ^ The 'Config' to use for this test. 'defaultVimConfig' is
        -- an example of a value we could provide.
        -> Assertion
runTest setupActions preConditions testActions assertions c = do
    let (setupEditor, a) = runEditor c setupActions emptyEditor
    preConditions setupEditor a
    let finalEditor = fst $ runEditor c (testActions a) setupEditor
    assertions finalEditor a


-- Return the contents of the current buffer as a string.
extractBufferString :: Config -> Editor -> String
extractBufferString c editor =
  R.toString $ snd (runEditor c (withCurrentBuffer elemsB) editor)


--------------------------------------------------
-- Functions for altering the state of the editor.

-- | Insert the given text into the editor inside an update transaction.
insertText :: String -> EditorM ()
insertText text =
    withCurrentBuffer $ do
        startUpdateTransactionB
        insertN (R.fromString text)
        commitUpdateTransactionB


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
assertContentOfCurrentBuffer :: Config -> String -> Editor -> Assertion
assertContentOfCurrentBuffer c expectedContent editor =
    assertEqual "Unexpected buffer content" expectedContent (extractBufferString c editor)


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
