{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defModeMapProto
    , VimBinding (..)
    , ModeMap (..)
    , vimEval
    , allBindings -- ^ for testing purposes
    , defaultVimEval -- ^ for testing purposes
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
import Yi.Keymap.Vim2.InsertMap
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.ReplaceMap
import Yi.Keymap.Vim2.ReplaceSingleCharMap
import Yi.Keymap.Vim2.Utils

data ModeMap = ModeMap {
        vimKeymap :: Keymap,
        normalMap :: [VimBinding],
        insertMap :: [VimBinding],
        replaceSingleMap :: [VimBinding],
        replaceMap :: [VimBinding]
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
                              insertMap = defInsertMap,
                              replaceSingleMap = defReplaceSingleMap,
                              replaceMap = defReplaceMap
                          }

defVimKeymap :: ModeMap -> KeymapM ()
defVimKeymap mm = do
    e <- anyEvent
    write $ handleEvent mm e

handleEvent :: ModeMap -> Event -> YiM ()
handleEvent mm e = do
    currentState <- withEditor getDynamic
    let maybeBinding = find (isBindingApplicable e currentState) (allBindings mm)
    case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show e
        Just (VimBindingY _ action) -> action e
        Just (VimBindingE _ action) -> withEditor $ action e
    withEditor $ do
        stateAfterAction <- getDynamic

        -- see comment for 'vimEval' below
        vimEval mm $ vsStringToEval stateAfterAction

        let newAccumulator = vsAccumulator stateAfterAction ++ eventToString e
        setDynamic $ stateAfterAction { vsAccumulator = newAccumulator }

allBindings :: ModeMap -> [VimBinding]
allBindings m = normalMap m ++ insertMap m ++ replaceSingleMap m ++ replaceMap m

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
    let maybeBinding = find (isBindingApplicable event currentState) (allPureBindings mm)
    case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show event
        Just (VimBindingE _ action) -> withEditor $ action event
        Just (VimBindingY _ _) -> fail "Impure binding found"

allPureBindings :: ModeMap -> [VimBinding]
allPureBindings mm = filter isPure $ allBindings mm
    where isPure (VimBindingE _ _) = True
          isPure _ = False
