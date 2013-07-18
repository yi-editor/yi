{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defVimConfig
    , VimBinding (..)
    , VimOperator (..)
    , VimConfig (..)
    , pureEval
    , impureEval
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
import Yi.Keymap.Vim2.Ex
import Yi.Keymap.Vim2.ExMap
import Yi.Keymap.Vim2.InsertMap
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.NormalOperatorPendingMap
import Yi.Keymap.Vim2.Operator
import Yi.Keymap.Vim2.ReplaceMap
import Yi.Keymap.Vim2.ReplaceSingleCharMap
import Yi.Keymap.Vim2.SearchMotionMap
import Yi.Keymap.Vim2.StateUtils
import Yi.Keymap.Vim2.Utils
import Yi.Keymap.Vim2.VisualMap

data VimConfig = VimConfig {
    vimKeymap :: Keymap
  , vimBindings :: [VimBinding]
  , vimOperators :: [VimOperator]
  , vimExCommandParsers :: [String -> Maybe ExCommand]
  }

mkKeymapSet :: Proto VimConfig -> KeymapSet
mkKeymapSet = modelessKeymapSet . vimKeymap . extractValue

keymapSet :: KeymapSet
keymapSet = mkKeymapSet defVimConfig

defVimConfig :: Proto VimConfig
defVimConfig = Proto $ \this -> VimConfig {
    vimKeymap = defVimKeymap this
  , vimBindings = concat
        [ defNormalMap (vimOperators this)
        , defNormalOperatorPendingMap (vimOperators this)
        , defExMap (vimExCommandParsers this)
        , defInsertMap
        , defReplaceSingleMap
        , defReplaceMap
        , defVisualMap (vimOperators this)
        , defSearchMotionMap
        ]
  , vimOperators = defOperators
  , vimExCommandParsers = defExCommandParsers
  }

defVimKeymap :: VimConfig -> KeymapM ()
defVimKeymap config = do
    e <- anyEvent
    write $ handleEvent config e

handleEvent :: VimConfig -> Event -> YiM ()
handleEvent config event = do
    currentState <- withEditor getDynamic
    let evs = vsBindingAccumulator currentState ++ eventToString event
        bindingMatch = selectBinding evs currentState (vimBindings config)
        prevMode = vsMode currentState

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

        performEvalIfNecessary config
        updateModeIndicatorE prevMode

-- This is not in Yi.Keymap.Vim2.Eval to avoid circular dependency:
-- eval needs to know about bindings, which contains normal bindings,
-- which contains '.', which needs to eval things
-- So as a workaround '.' just saves a string that needs eval in VimState
-- and the actual evaluation happens in handleEvent
pureEval :: VimConfig -> String -> EditorM ()
pureEval config s = sequence_ actions
        where actions = map (pureHandleEvent config) $ parseEvents s

impureEval :: VimConfig -> String -> YiM ()
impureEval config s = sequence_ actions
        where actions = map (handleEvent config) $ parseEvents s

pureHandleEvent :: VimConfig -> Event -> EditorM ()
pureHandleEvent config event = do
    currentState <- getDynamic
    let evs = vsBindingAccumulator currentState ++ eventToString event
        bindingMatch = selectBinding evs currentState (allPureBindings config)
        prevMode = vsMode currentState
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

    performEvalIfNecessary config
    updateModeIndicatorE prevMode

performEvalIfNecessary :: VimConfig -> EditorM ()
performEvalIfNecessary config = do
    stateAfterAction <- getDynamic

    -- see comment for 'pureEval'
    modifyStateE $ \s -> s { vsStringToEval = "" }
    pureEval config $ vsStringToEval stateAfterAction

allPureBindings :: VimConfig -> [VimBinding]
allPureBindings config = filter isPure $ vimBindings config
    where isPure (VimBindingE _ _) = True
          isPure _ = False
