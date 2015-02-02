{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.ReplaceMap
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.ReplaceMap (defReplaceMap) where

import           Control.Monad            (replicateM_, when)
import           Data.Monoid              (Monoid (mempty), (<>))
import qualified Data.Text                as T (unpack)
import           Yi.Buffer.Adjusted
import           Yi.Editor                (EditorM, getEditorDyn, withCurrentBuffer)
import           Yi.Keymap.Keys           (Key (KEsc), ctrlCh, spec)
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.EventUtils (eventToEventString, parseEvents)
import           Yi.Keymap.Vim.StateUtils
import           Yi.Keymap.Vim.Utils      (mkBindingE)

defReplaceMap :: [VimBinding]
defReplaceMap = specials <> [printable]

specials :: [VimBinding]
specials = fmap (mkBindingE Replace Finish)
             [ (spec KEsc, exitReplaceMode, resetCount . switchMode Normal)
             , (ctrlCh 'c', exitReplaceMode, resetCount . switchMode Normal)
             ]

exitReplaceMode :: EditorM ()
exitReplaceMode = do
  count <- getCountE
  when (count > 1) $ do
      inputEvents <- fmap (parseEvents . vsOngoingInsertEvents) getEditorDyn
      replicateM_ (count - 1) $ mapM_ (printableAction . eventToEventString) inputEvents
  modifyStateE $ \s -> s { vsOngoingInsertEvents = mempty }
  withCurrentBuffer $ moveXorSol 1

printable :: VimBinding
printable = VimBindingE f
    where f evs s | Replace == vsMode s = WholeMatch $ printableAction evs
          f _ _ = NoMatch

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    saveInsertEventStringE evs
    withCurrentBuffer $ case T.unpack . _unEv $ evs of
        [c]    -> insertOrReplaceB c
        "<CR>" -> insertOrReplaceB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        "<Esc>" -> replicateM_ 4 $ insertOrReplaceB ' '
        "<C-t>" -> return () -- TODO
        "<C-d>" -> return () -- TODO
        "<C-e>" -> insertOrReplaceCharWithBelowB
        "<C-y>" -> insertOrReplaceCharWithAboveB
        "<C-h>" -> return () -- TODO
        "<C-j>" -> return () -- TODO
        "<C-o>" -> return () -- TODO
        "<C-w>" -> return () -- TODO
        "<C-r>" -> return () -- TODO
        "<C-k>" -> return () -- TODO
        evs' -> error $ "Unhandled event " <> evs' <> " in replace mode"
    return Continue

insertOrReplaceB :: Char -> BufferM ()
insertOrReplaceB c = do
    currentChar <- readB
    if currentChar == '\n'
    then insertB c
    else replaceCharB c
    rightB

insertOrReplaceCharWithBelowB :: BufferM ()
insertOrReplaceCharWithBelowB = do
    currentChar <- readB
    if currentChar == '\n'
    then insertCharWithBelowB
    else replaceCharWithBelowB
    rightB

insertOrReplaceCharWithAboveB :: BufferM ()
insertOrReplaceCharWithAboveB = do
    currentChar <- readB
    if currentChar == '\n'
    then insertCharWithAboveB
    else replaceCharWithAboveB
    rightB
