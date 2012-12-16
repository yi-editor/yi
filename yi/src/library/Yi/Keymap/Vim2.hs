{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defModeMapProto
    , VimBinding (..)
    , ModeMap (..)
    , vimEval
    -- | for testing purposes
    , allBindings
    , defaultVimEval
    ) where

import Prelude ()
import Yi.Prelude

import Data.List (map, filter)
import Data.Prototype

import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Keys (anyEvent)

import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.EventUtils
import Yi.Keymap.Vim2.InsertMap
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.NormalOperatorPendingMap
import Yi.Keymap.Vim2.ReplaceMap
import Yi.Keymap.Vim2.ReplaceSingleCharMap
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.VisualMap

data ModeMap = ModeMap {
        vimKeymap :: Keymap,
        normalMap :: [VimBinding],
        normalOperatorPendingMap :: [VimBinding],
        insertMap :: [VimBinding],
        replaceSingleMap :: [VimBinding],
        replaceMap :: [VimBinding],
        visualMap :: [VimBinding]
    }

mkKeymapSet :: Proto ModeMap -> KeymapSet
mkKeymapSet = modelessKeymapSet . vimKeymap . extractValue

keymapSet :: KeymapSet
keymapSet = mkKeymapSet defModeMapProto

defModeMapProto :: Proto ModeMap
defModeMapProto = Proto template
    where template self = ModeMap {
                              vimKeymap = defVimKeymap self,
                              normalMap = defNormalMap,
                              normalOperatorPendingMap = defNormalOperatorPendingMap,
                              insertMap = defInsertMap,
                              replaceSingleMap = defReplaceSingleMap,
                              replaceMap = defReplaceMap,
                              visualMap = defVisualMap
                          }

defVimKeymap :: ModeMap -> KeymapM ()
defVimKeymap mm = do
    e <- anyEvent
    write $ handleEvent mm e

handleEvent :: ModeMap -> Event -> YiM ()
handleEvent mm event = do
    currentState <- withEditor getDynamic
    let evs = vsBindingAccumulator currentState ++ eventToString event
    let bindingMatch = selectBinding evs currentState (allBindings mm)

    repeatToken <- case bindingMatch of
        WholeMatch (VimBindingY _ action) -> do
            withEditor dropBindingAccumulatorE
            action evs
        WholeMatch (VimBindingE _ action) -> withEditor $ dropBindingAccumulatorE >> action evs
        NoMatch -> do
            withEditor dropBindingAccumulatorE
            return Drop
        PartialMatch -> do
            withEditor $ accumulateBindingEventE event
            return Continue

    withEditor $ do
        case repeatToken of
            Drop -> dropAccumulatorE
            Continue -> accumulateEventE event
            Finish -> do
                accumulateEventE event
                flushAccumulatorIntoRepeatableActionE

        performEvalIfNecessary mm

allBindings :: ModeMap -> [VimBinding]
allBindings m = concat [ normalMap m
                       , normalOperatorPendingMap m
                       , insertMap m
                       , replaceSingleMap m
                       , replaceMap m
                       , visualMap m
                       ]

-- This is not in Yi.Keymap.Vim2.Eval to avoid circular dependency:
-- eval needs to know about bindings, which contains normal bindings,
-- which contains '.', which needs to eval things
-- So as a workaround '.' just saves a string that needs eval in VimState
-- and the actual evaluation happens in handleEvent
vimEval :: ModeMap -> String -> EditorM ()
vimEval mm s = sequence_ actions
        where actions = map (pureHandleEvent mm) $ parseEvents s

defaultVimEval :: String -> EditorM ()
defaultVimEval = vimEval $ extractValue defModeMapProto

pureHandleEvent :: ModeMap -> Event -> EditorM ()
pureHandleEvent mm event = do
    currentState <- getDynamic
    let evs = vsBindingAccumulator currentState ++ eventToString event
        bindingMatch = selectBinding evs currentState (allPureBindings mm)
    case bindingMatch of
        NoMatch -> dropBindingAccumulatorE
        PartialMatch -> do
            accumulateBindingEventE event
            accumulateEventE event
        WholeMatch (VimBindingE _ action) -> do
            repeatToken <- withEditor $ action evs
            dropBindingAccumulatorE
            case repeatToken of
                Drop -> dropAccumulatorE
                Continue -> accumulateEventE event
                Finish -> accumulateEventE event >> flushAccumulatorIntoRepeatableActionE
        WholeMatch (VimBindingY _ _) -> fail "Impure binding found"

    performEvalIfNecessary mm

performEvalIfNecessary :: ModeMap -> EditorM ()
performEvalIfNecessary mm = do
    stateAfterAction <- getDynamic

    -- see comment for 'vimEval' below
    modifyStateE $ \s -> s { vsStringToEval = "" }
    vimEval mm $ vsStringToEval stateAfterAction

allPureBindings :: ModeMap -> [VimBinding]
allPureBindings mm = filter isPure $ allBindings mm
    where isPure (VimBindingE _ _) = True
          isPure _ = False
