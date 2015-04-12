{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The vim keymap.

module Yi.Keymap.Vim
    ( keymapSet
    , mkKeymapSet
    , defVimConfig
    , VimBinding (..)
    , VimOperator (..)
    , VimConfig (..)
    , pureEval
    , impureEval
    , relayoutFromTo
    ) where

import Control.Applicative                    ((<$>))
import Data.Char                              (toUpper)
import Data.List                              (find)
import Data.Monoid                            (Monoid (mempty), (<>))
import Data.Prototype                         (Proto (Proto), extractValue)
import Yi.Buffer.Adjusted                     (commitUpdateTransactionB, startUpdateTransactionB)
import Yi.Editor
import Yi.Event                               (Event (..), Key (KASCII), Modifier (MCtrl, MMeta))
import Yi.Keymap                              (Keymap, KeymapM, KeymapSet, YiM, modelessKeymapSet, write)
import Yi.Keymap.Keys                         (anyEvent)
import Yi.Keymap.Vim.Common
import Yi.Keymap.Vim.Digraph                  (defDigraphs)
import Yi.Keymap.Vim.EventUtils               (eventToEventString, parseEvents)
import Yi.Keymap.Vim.Ex                       (ExCommand, defExCommandParsers)
import Yi.Keymap.Vim.ExMap                    (defExMap)
import Yi.Keymap.Vim.InsertMap                (defInsertMap)
import Yi.Keymap.Vim.NormalMap                (defNormalMap)
import Yi.Keymap.Vim.NormalOperatorPendingMap (defNormalOperatorPendingMap)
import Yi.Keymap.Vim.Operator                 (VimOperator (..), defOperators)
import Yi.Keymap.Vim.ReplaceMap               (defReplaceMap)
import Yi.Keymap.Vim.ReplaceSingleCharMap     (defReplaceSingleMap)
import Yi.Keymap.Vim.SearchMotionMap          (defSearchMotionMap)
import Yi.Keymap.Vim.StateUtils
import Yi.Keymap.Vim.Utils                    (selectBinding, selectPureBinding)
import Yi.Keymap.Vim.VisualMap                (defVisualMap)

data VimConfig = VimConfig {
    vimKeymap           :: Keymap
  , vimBindings         :: [VimBinding]
  , vimOperators        :: [VimOperator]
  , vimExCommandParsers :: [EventString -> Maybe ExCommand]
  , vimDigraphs         :: [(String, Char)]
  , vimRelayout         :: Char -> Char
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
  , vimRelayout = id
  }

defVimKeymap :: VimConfig -> KeymapM ()
defVimKeymap config = do
  e <- anyEvent
  write $ impureHandleEvent config e True

-- This is not in Yi.Keymap.Vim.Eval to avoid circular dependency:
-- eval needs to know about bindings, which contains normal bindings,
-- which contains '.', which needs to eval things
-- So as a workaround '.' just saves a string that needs eval in VimState
-- and the actual evaluation happens in impureHandleEvent
pureEval :: VimConfig -> EventString -> EditorM ()
pureEval config = sequence_ . map (pureHandleEvent config) . parseEvents

impureEval :: VimConfig -> EventString -> Bool -> YiM ()
impureEval config s needsToConvertEvents = sequence_ actions
  where actions = map (\e -> impureHandleEvent config e needsToConvertEvents) $ parseEvents s

pureHandleEvent :: VimConfig -> Event -> EditorM ()
pureHandleEvent config ev
    = genericHandleEvent allPureBindings selectPureBinding config ev False

impureHandleEvent :: VimConfig -> Event -> Bool -> YiM ()
impureHandleEvent = genericHandleEvent vimBindings selectBinding

genericHandleEvent :: MonadEditor m => (VimConfig -> [VimBinding])
                   -> (EventString -> VimState -> [VimBinding]
                       -> MatchResult (m RepeatToken))
                   -> VimConfig
                   -> Event
                   -> Bool
                   -> m ()
genericHandleEvent getBindings pick config unconvertedEvent needsToConvertEvents = do
    currentState <- withEditor getEditorDyn
    let event = if needsToConvertEvents
                then convertEvent (vsMode currentState) (vimRelayout config) unconvertedEvent
                else unconvertedEvent
        evs = vsBindingAccumulator currentState <> eventToEventString event
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
        newMode <- vsMode <$> getEditorDyn

        -- TODO: we should introduce some hook mechanism like autocommands in vim
        case (prevMode, newMode) of
            (Insert _, Insert _) -> return ()
            (Insert _, _) -> withCurrentBuffer commitUpdateTransactionB
            (_, Insert _) -> withCurrentBuffer startUpdateTransactionB
            _ -> return ()

        performEvalIfNecessary config
        updateModeIndicatorE currentState

performEvalIfNecessary :: VimConfig -> EditorM ()
performEvalIfNecessary config = do
    stateAfterAction <- getEditorDyn

    -- see comment for 'pureEval'
    modifyStateE $ \s -> s { vsStringToEval = mempty }
    pureEval config (vsStringToEval stateAfterAction)

allPureBindings :: VimConfig -> [VimBinding]
allPureBindings config = filter isPure $ vimBindings config
    where isPure (VimBindingE _) = True
          isPure _ = False

convertEvent :: VimMode -> (Char -> Char) -> Event -> Event
convertEvent (Insert _) f (Event (KASCII c) mods)
    | MCtrl `elem` mods || MMeta `elem` mods = Event (KASCII (f c)) mods
convertEvent Ex _ e = e
convertEvent (Insert _) _ e = e
convertEvent InsertNormal _ e = e
convertEvent InsertVisual _ e = e
convertEvent Replace _ e = e
convertEvent ReplaceSingleChar _ e = e
convertEvent (Search _ _) _ e = e
convertEvent _ f (Event (KASCII c) mods) = Event (KASCII (f c)) mods
convertEvent _ _ e = e

relayoutFromTo :: String -> String -> (Char -> Char)
relayoutFromTo keysFrom keysTo = \c ->
    maybe c fst (find ((== c) . snd)
                      (zip (keysTo ++ fmap toUpper' keysTo)
                           (keysFrom ++ fmap toUpper' keysFrom)))
    where toUpper' ';' = ':'
          toUpper' a = toUpper a
