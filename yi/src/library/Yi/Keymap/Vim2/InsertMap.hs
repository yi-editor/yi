module Yi.Keymap.Vim2.InsertMap
  ( defInsertMap
  ) where

import Yi.Prelude
import Prelude ()

import Control.Monad (replicateM_)

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.StateUtils

defInsertMap :: [VimBinding]
defInsertMap = specials ++ [printable]

specials :: [VimBinding]
specials = fmap (mkBindingE Insert Finish)
             [ (spec KEsc, exitInsertMode, resetCount . switchMode Normal)
             , (ctrlCh 'c', exitInsertMode, resetCount . switchMode Normal)
             ]

exitInsertMode :: EditorM ()
exitInsertMode = do
    count <- getCountE
    when (count > 1) $ do
        inputEvents <- fmap (parseEvents . vsOngoingInsertEvents) getDynamic
        replicateM_ (count - 1) $ mapM_ printableAction inputEvents
    modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
    withBuffer0 $ moveXorSol 1

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = Insert == vsMode s
          action = printableAction

printableAction :: Event -> EditorM RepeatToken
printableAction e = do
    save e
    withBuffer0 $ case e of
        (Event (KASCII c) []) -> insertB c
        (Event KEnter []) -> insertB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        (Event KTab []) -> insertN $ replicate 4 ' '
        (Event (KASCII 't') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'd') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'e') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'y') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'h') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'j') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'o') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'w') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'r') [MCtrl]) -> return () -- TODO
        (Event (KASCII 'k') [MCtrl]) -> return () -- TODO
        e' -> error $ "Unhandled event " ++ show e'
    return Continue

save :: Event -> EditorM ()
save e =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ eventToString e }
