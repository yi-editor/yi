{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Yi.Keymap.Vim2
    ( keymapSet
    , mkKeymapSet
    , defModeMapProto
    , VimBinding(..)
    , ModeMap(..)
    ) where
    
import Prelude ()
import Yi.Prelude

import Data.Prototype

import Yi.Editor
import Yi.Keymap
import Yi.Keymap.Keys
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.InsertMap


data ModeMap = ModeMap {
        vimKeymap :: Keymap,
        normalMap :: [VimBinding],
        insertMap :: [VimBinding]
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
                              insertMap = defInsertMap
                          }

defVimKeymap :: ModeMap -> KeymapM ()
defVimKeymap m = do
    e <- anyEvent
    let maybeBinding = find ((== e) . vbEvent) allBindings
        allBindings = normalMap m ++ insertMap m
    case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show e
        Just (VimBinding _ prereq action mutateState) -> write $ do
            currentState <- withEditor getDynamic
            when (prereq currentState) $ do
              action
              withEditor $ setDynamic $ mutateState currentState

