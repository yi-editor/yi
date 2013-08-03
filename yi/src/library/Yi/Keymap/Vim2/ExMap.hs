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

defExMap :: [String -> Maybe ExCommand] -> [VimBinding]
defExMap cmdParsers =
    [ exitBinding
    , completionBinding cmdParsers
    , finishBindingY cmdParsers
    , finishBindingE cmdParsers
    , failBindingE
    , printable
    ]

completionBinding :: [String -> Maybe ExCommand] -> VimBinding
completionBinding commandParsers = VimBindingY prereq action
    where prereq evs (VimState { vsMode = Ex }) = matchFromBool $ evs == "<Tab>"
          prereq _ _ = NoMatch
          action _ = do
              commandString <- withEditor $ withBuffer0 elemsB
              case stringToExCommand commandParsers commandString of
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
    where prereq "<CR>" (VimState { vsMode = Ex, vsOngoingInsertEvents = [] })
              = WholeMatch ()
          prereq evs (VimState { vsMode = Ex })
              = matchFromBool $ evs `elem` ["<Esc>", "<C-c>"]
          prereq _ _ = NoMatch
          action _ = do
              exitEx
              return Drop

finishBindingY :: [String -> Maybe ExCommand] -> VimBinding
finishBindingY commandParsers = VimBindingY
    (finishPrereq commandParsers (not . cmdIsPure))
    (const $ finishAction commandParsers exEvalY)

finishBindingE :: [String -> Maybe ExCommand] -> VimBinding
finishBindingE commandParsers = VimBindingE
    (finishPrereq commandParsers cmdIsPure)
    (const $ finishAction commandParsers exEvalE)

finishPrereq :: [String -> Maybe ExCommand] -> (ExCommand -> Bool)
    -> EventString -> VimState -> MatchResult ()
finishPrereq commandParsers cmdPred evs s =
    matchFromBool . and $
        [ vsMode s == Ex
        , evs == "<CR>"
        , case stringToExCommand commandParsers (vsOngoingInsertEvents s) of
            Just cmd -> cmdPred cmd
            _ -> False
        ]

finishAction :: MonadEditor m => [String -> Maybe ExCommand] ->
    ([String -> Maybe ExCommand] -> String -> m ()) -> m RepeatToken
finishAction commandParsers execute = do
    s <- withEditor $ withBuffer0 elemsB
    withEditor exitEx
    execute commandParsers s
    return Drop

failBindingE :: VimBinding
failBindingE = VimBindingE prereq action
    where prereq evs s = matchFromBool . and $ [vsMode s == Ex, evs == "<CR>"]
          action _ = do
              exitEx
              printMsg "Unknown command"
              return Drop

printable :: VimBinding
printable = VimBindingE prereq editAction
    where prereq _ (VimState { vsMode = Ex }) = WholeMatch ()
          prereq _ _ = NoMatch

editAction :: EventString -> EditorM RepeatToken
editAction evs = do
    let bufAction = case evs of
            (c:[]) -> insertB c
            "<BS>"  -> deleteB Character Backward
            "<C-h>" -> deleteB Character Backward
            "<C-w>" -> do
                r <- regionOfPartNonEmptyB unitViWordOnLine Backward
                deleteRegionB r
            "<C-r>" -> return () -- TODO
            "<lt>" -> insertB '<'
            "<Del>" -> deleteB Character Forward
            "<Left>" -> moveXorSol 1
            "<C-b>" -> moveXorSol 1
            "<Right>" -> moveXorEol 1
            "<C-f>" -> moveXorEol 1
            "<Home>" -> moveToSol
            "<C-a>" -> moveToSol
            "<End>" -> moveToEol
            "<C-e>" -> moveToEol
            "<C-u>" -> moveToSol >> deleteToEol
            "<C-k>" -> deleteToEol
            evs' -> error $ "Unhandled event " ++ evs' ++ " in ex mode"
    command <- withBuffer0 $ bufAction >> elemsB
    modifyStateE $ \state -> state {
        vsOngoingInsertEvents = command
    }
    return Continue
