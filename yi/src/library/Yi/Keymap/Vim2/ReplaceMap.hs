module Yi.Keymap.Vim2.ReplaceMap
    ( defReplaceMap
    ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Yi.Buffer
import Yi.Editor
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils

defReplaceMap :: [VimBinding]
defReplaceMap = specials ++ [printable]

specials :: [VimBinding]
specials = fmap (mkBindingE Replace Finish)
             [ (spec KEsc, exitReplaceMode, resetCount . switchMode Normal)
             , (ctrlCh 'c', exitReplaceMode, resetCount . switchMode Normal)
             ]

exitReplaceMode :: EditorM ()
exitReplaceMode = do
    count <- getCountE
    when (count > 1) $ do
        inputEvents <- fmap (parseEvents . vsOngoingInsertEvents) getDynamic
        replicateM_ (count - 1) $ mapM_ (printableAction . eventToString) inputEvents
    modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
    withBuffer0 $ moveXorSol 1

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = matchFromBool $ Replace == vsMode s
          action = printableAction

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    saveInsertEventStringE evs
    withBuffer0 $ case evs of
        (c:[]) -> insertOrReplaceB c
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
        evs' -> error $ "Unhandled event " ++ evs' ++ " in replace mode"
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
