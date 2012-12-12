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
        replicateM_ (count - 1) $ mapM_ printableAction inputEvents
    modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
    withBuffer0 $ moveXorSol 1

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = matchFromBool $ Replace == vsMode s
          action = printableAction

printableAction :: Event -> EditorM RepeatToken
printableAction e = do
    save e
    withBuffer0 $ case e of
        (Event (KASCII c) []) -> insertOrReplaceB c
        (Event KEnter []) -> insertOrReplaceB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        (Event KTab []) -> replicateM_ 4 $ insertOrReplaceB ' '
        (Event (KASCII 't') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'd') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'e') [MCtrl]) -> insertOrReplaceCharWithBelowB
        (Event (KASCII 'y') [MCtrl]) -> insertOrReplaceCharWithAboveB
        (Event (KASCII 'h') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'j') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'o') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'w') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'r') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'k') [MCtrl]) -> return () -- TODO
        e' -> error $ "Unhandled event " ++ show e' ++ " in replace mode"
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

save :: Event -> EditorM ()
save e =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ eventToString e }
