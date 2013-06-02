module Yi.Keymap.Vim2.ExMap
    ( defExMap
    ) where

import Prelude ()
import Yi.Prelude

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.Ex

defExMap :: [VimBinding]
defExMap = [ exitBinding, completionBinding
           , finishBindingY, finishBindingE
           , failBindingE
           , printable
           ]

completionBinding :: VimBinding
completionBinding = VimBindingY prereq action
    where prereq evs (VimState { vsMode = Ex }) = matchFromBool $ evs == "<Tab>"
          prereq _ _ = NoMatch
          action _ = do
              commandString <- withEditor $ withBuffer0 elemsB
              case stringToExCommand allExCommands commandString of
                  Just cmd -> do
                      maybeNewString <- cmdComplete cmd
                      case maybeNewString of
                        Just s -> withBuffer $ replaceBufferContent s
                        Nothing -> return ()
                  Nothing -> return ()
              return Drop

exitEx :: EditorM ()
exitEx = do
    resetCountE
    switchModeE Normal
    closeBufferAndWindowE

exitBinding :: VimBinding
exitBinding = VimBindingE prereq action
    where prereq evs (VimState { vsMode = Ex }) =
              matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              exitEx
              return Drop

finishBindingY :: VimBinding
finishBindingY = VimBindingY
    (finishPrereq (not . cmdIsPure))
    (const $ finishAction exEvalY)

finishBindingE :: VimBinding
finishBindingE = VimBindingE
    (finishPrereq cmdIsPure)
    (const $ finishAction exEvalE)

finishPrereq :: (ExCommandBox -> Bool) -> EventString -> VimState -> MatchResult ()
finishPrereq cmdPred evs s =
    matchFromBool . and $
        [ vsMode s == Ex
        , evs == "<CR>"
        , case stringToExCommand allExCommands (vsOngoingInsertEvents s) of
            Just cmd -> cmdPred cmd
            _ -> False
        ]

finishAction :: MonadEditor m => ([ExCommandBox] -> String -> m ()) -> m RepeatToken
finishAction execute = do
    s <- withEditor $ withBuffer0 elemsB
    withEditor exitEx
    execute allExCommands s
    return Drop

failBindingE :: VimBinding
failBindingE = VimBindingE prereq action
    where prereq evs s = matchFromBool . and $ [vsMode s == Ex, evs == "<CR>"]
          action _ = do
              exitEx
              printMsg "Unknown command"
              return Drop

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
