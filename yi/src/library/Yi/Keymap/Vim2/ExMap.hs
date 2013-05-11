module Yi.Keymap.Vim2.ExMap
    ( defExMap
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer hiding (Insert)
import Yi.Core (closeWindow)
import Yi.Editor
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.Ex

defExMap :: [VimBinding]
defExMap = [exitBinding, finishBindingY, finishBindingE, printable]

exitBinding :: VimBinding
exitBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = Ex }) =
              matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              resetCountE
              switchModeE Normal
              withEditor closeBufferAndWindowE
              return Drop

finishBindingY :: VimBinding
finishBindingY = VimBindingY prereq action
    where prereq evs s =
              matchFromBool . and $
                [ vsMode s == Ex
                , evs == "<CR>"
                , vsOngoingInsertEvents s `elem` ["q!", "quit!"]
                ]
          action evs = do
              withEditor $ do
                  resetCountE
                  switchModeE Normal
                  closeBufferAndWindowE
              closeWindow
              return Drop

finishBindingE :: VimBinding
finishBindingE = VimBindingE prereq action
    where prereq evs (VimState { vsMode = Ex }) =
              matchFromBool $ evs == "<CR>"
          prereq _ _ = NoMatch
          action _ = do
              resetCountE
              switchModeE Normal
              s <- withBuffer0 elemsB
              closeBufferAndWindowE
              exEvalE s
              return Finish

printable :: VimBinding
printable = VimBindingE prereq printableAction
    where prereq _ (VimState { vsMode = Ex }) = WholeMatch ()
          prereq _ _ = NoMatch

printableAction :: EventString -> EditorM RepeatToken
printableAction evs = do
    let bufAction = case evs of
            (c:[]) -> insertB c
            "<BS>"  -> deleteB Character Backward
            "<C-h>" -> deleteB Character Backward
            "<C-w>" -> do
                r <- regionOfPartNonEmptyB unitViWordOnLine Backward
                deleteRegionB r
            "<C-r>" -> return () -- TODO
            "<lt>" -> insertB '<'
            evs' -> error $ "Unhandled event " ++ evs' ++ " in ex mode"
    command <- withBuffer0 $ bufAction >> elemsB
    modifyStateE $ \state -> state {
        vsOngoingInsertEvents = command
    }
    return Continue
