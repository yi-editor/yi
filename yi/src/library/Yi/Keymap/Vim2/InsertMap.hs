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
defInsertMap = [exitBinding, printable]

exitBinding :: VimBinding
exitBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = (Insert _) }) =
              matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              count <- getCountE
              (Insert starter) <- fmap vsMode getDynamic
              when (count > 1) $ do
                  inputEvents <- fmap (parseEvents . vsOngoingInsertEvents) getDynamic
                  replicateM_ (count - 1) $ do
                      when (starter `elem` "Oo") $ withBuffer0 $ insertB '\n'
                      mapM_ (printableAction . eventToString) inputEvents
              modifyStateE $ \s -> s { vsOngoingInsertEvents = "" }
              withBuffer0 $ moveXorSol 1
              resetCountE
              switchModeE Normal
              return Finish

printable :: VimBinding
printable = VimBindingE prereq action
    where prereq _ (VimState { vsMode = Insert _ } ) = WholeMatch ()
          prereq _ _ = NoMatch
          action = printableAction

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    save evs
    withBuffer0 $ case evs of
        (c:[]) -> insertB c
        "<CR>" -> insertB '\n'
        -- For testing purposes assume noexpandtab, tw=4
        "<Tab>" -> insertN $ replicate 4 ' '
        "<C-t>" -> shiftIndentOfRegion 1 =<< regionOfB Line
        "<C-d>" -> shiftIndentOfRegion (-1) =<< regionOfB Line
        "<C-e>" -> insertCharWithBelowB
        "<C-y>" -> insertCharWithAboveB
        "<BS>"  -> deleteB Character Backward
        "<C-h>" -> deleteB Character Backward
        "<C-j>" -> insertB '\n'
        "<C-o>" -> return () -- TODO
        "<C-w>" -> deleteRegionB =<< regionOfPartNonEmptyB unitViWordOnLine Backward
        "<C-r>" -> return () -- TODO
        "<C-k>" -> return () -- TODO
        evs' -> error $ "Unhandled event " ++ evs' ++ " in insert mode"
    return Continue

save :: EventString -> EditorM ()
save evs =
    modifyStateE $ \s -> s { vsOngoingInsertEvents = vsOngoingInsertEvents s ++ evs }
