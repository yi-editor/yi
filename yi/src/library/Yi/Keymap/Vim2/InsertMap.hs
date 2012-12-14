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
        replicateM_ (count - 1) $ mapM_ (printableAction . eventToString) inputEvents
    modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
    withBuffer0 $ moveXorSol 1

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ s = matchFromBool $ Insert == vsMode s
          action = printableAction

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    save evs
    withBuffer0 $ case evs of
        (c:[]) -> insertB c
        "<CR>" -> insertB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        "<Tab>" -> insertN $ replicate 4 ' '
        "<C-t>" -> return () -- TODO
        "<C-d>" -> return () -- TODO
        "<C-e>" -> insertCharWithBelowB
        "<C-y>" -> insertCharWithAboveB
        "<C-h>" -> deleteB Character Backward
        "<C-j>" -> return () -- TODO
        "<C-o>" -> return () -- TODO
        "<C-w>" -> return () -- TODO
        "<C-r>" -> return () -- TODO
        "<C-k>" -> return () -- TODO
        evs' -> error $ "Unhandled event " ++ evs' ++ " in insert mode"
    return Continue

save :: EventString -> EditorM ()
save evs =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ evs }
