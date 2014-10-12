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

import Control.Applicative
import Data.Char (toUpper)
import Data.List (find)
import Data.Monoid
import Data.Prototype
import Yi.Buffer.Adjusted hiding (Insert)
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
  , vimExCommandParsers :: [EventString -> Maybe ExCommand]
  , vimDigraphs :: [(String, Char)]
  , vimRelayout :: Char -> Char
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
--
-- TODO: pass through untouched 'EventString' to 'parseEvents' instead
-- of converting to 'String'.
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
    currentState <- withEditor getDynamic
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
        newMode <- vsMode <$> getDynamic

        -- TODO: we should introduce some hook mechanism like autocommands in vim
        case (prevMode, newMode) of
            (Insert _, Insert _) -> return ()
            (Insert _, _) -> withCurrentBuffer commitUpdateTransactionB
            (_, Insert _) -> withCurrentBuffer startUpdateTransactionB
            _ -> return ()

        performEvalIfNecessary config
        updateModeIndicatorE prevMode

performEvalIfNecessary :: VimConfig -> EditorM ()
performEvalIfNecessary config = do
    stateAfterAction <- getDynamic

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
