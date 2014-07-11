{-# LANGUAGE ScopedTypeVariables #-}

module Yi.Keymap.Vim
    ( keymapSet
    , mkKeymapSet
    , defVimConfig
    , VimBinding (..)
    , VimOperator (..)
    , VimConfig (..)
    , pureEval
    , impureEval
    ) where

import Control.Applicative
import Data.Prototype

import Yi.Buffer hiding (Insert)
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Keymap.Keys (anyEvent)

import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Digraph
import Yi.Keymap.Vim.EventUtils
import Yi.Keymap.Vim.Ex
import Yi.Keymap.Vim.ExMap
import Yi.Keymap.Vim.InsertMap
import Yi.Keymap.Vim.NormalMap
import Yi.Keymap.Vim.NormalOperatorPendingMap
import Yi.Keymap.Vim.Operator
import Yi.Keymap.Vim.ReplaceMap
import Yi.Keymap.Vim.ReplaceSingleCharMap
import Yi.Keymap.Vim.SearchMotionMap
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Utils
import Yi.Keymap.Vim.VisualMap

data VimConfig = VimConfig {
    vimKeymap :: Keymap
  , vimBindings :: [VimBinding]
  , vimOperators :: [VimOperator]
  , vimExCommandParsers :: [String -> Maybe ExCommand]
  , vimDigraphs :: [(String, Char)]
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
        , defInsertMap (vimDigraphs this)
        , defReplaceSingleMap
        , defReplaceMap
        , defVisualMap (vimOperators this)
        , defSearchMotionMap
        ]
  , vimOperators = defOperators
  , vimExCommandParsers = defExCommandParsers
  , vimDigraphs = defDigraphs
  }

defVimKeymap :: VimConfig -> KeymapM ()
defVimKeymap config = do
    e <- anyEvent
    write $ impureHandleEvent config e

-- This is not in Yi.Keymap.Vim.Eval to avoid circular dependency:
-- eval needs to know about bindings, which contains normal bindings,
-- which contains '.', which needs to eval things
-- So as a workaround '.' just saves a string that needs eval in VimState
-- and the actual evaluation happens in impureHandleEvent
pureEval :: VimConfig -> String -> EditorM ()
pureEval config s = sequence_ actions
        where actions = map (pureHandleEvent config) $ parseEvents s

impureEval :: VimConfig -> String -> YiM ()
impureEval config s = sequence_ actions
        where actions = map (impureHandleEvent config) $ parseEvents s

pureHandleEvent :: VimConfig -> Event -> EditorM ()
pureHandleEvent = genericHandleEvent allPureBindings selectPureBinding

impureHandleEvent :: VimConfig -> Event -> YiM ()
impureHandleEvent = genericHandleEvent vimBindings selectBinding

genericHandleEvent :: MonadEditor m
    => (VimConfig -> [VimBinding])
    -> (EventString -> VimState -> [VimBinding] -> MatchResult (m RepeatToken))
    -> VimConfig -> Event -> m ()
genericHandleEvent getBindings pick config event = do
    currentState <- withEditor getDynamic
    let evs = vsBindingAccumulator currentState ++ eventToString event
        bindingMatch = pick evs currentState (getBindings config)
        prevMode = vsMode currentState

    case bindingMatch of
        NoMatch -> withEditor dropBindingAccumulatorE
        PartialMatch -> withEditor $ do
            accumulateBindingEventE event
            accumulateEventE event
        WholeMatch action -> do
            repeatToken <- action
            withEditor $ do
                dropBindingAccumulatorE
                accumulateEventE event
                case repeatToken of
                    Drop -> do
                        resetActiveRegisterE
                        dropAccumulatorE
                    Continue -> return ()
                    Finish -> do
                        resetActiveRegisterE
                        flushAccumulatorE

    withEditor $ do
        newMode <- vsMode <$> getDynamic

        -- TODO: we should introduce some hook mechanism like autocommands in vim
        case (prevMode, newMode) of
            (Insert _, Insert _) -> return ()
            (Insert _, _) -> withBuffer0 commitUpdateTransactionB
            (_, Insert _) -> withBuffer0 startUpdateTransactionB
            _ -> return ()

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
    where isPure (VimBindingE _) = True
          isPure _ = False
