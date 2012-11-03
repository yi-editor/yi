{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defModeMapProto
    , VimBinding (..)
    , ModeMap (..)
    , allBindings
    ) where

import Prelude ()
import Yi.Prelude

import Data.Prototype

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
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

allBindings :: ModeMap -> [VimBinding]
allBindings m = normalMap m ++ insertMap m ++ replaceSingleMap m ++ replaceMap m
