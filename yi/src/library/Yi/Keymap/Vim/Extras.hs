{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      :  Yi.Keymap.Vim.Extras
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- I'm a module waiting for some kind soul to give me a commentary!

module Yi.Keymap.Vim.Extras ( nmap, imap, leader, key, v2KeymapSet ) where

import Yi ( EditorM(..)
          , BufferM(..)
          , KeymapSet(..)
          , YiM(..)
          , override
          , withBuffer0
          , defaultVimConfig
          )

import Data.Monoid ( mappend )

import Yi.Keymap.Vim        ( VimConfig, defVimConfig, vimBindings, mkKeymapSet, pureEval )
import Yi.Keymap.Vim.Common ( EventString
                            , VimBinding(VimBindingE)
                            , VimMode(Normal, Insert)
                            , RepeatToken(Drop, Continue)
                            , MatchResult(NoMatch)
                            , vsMode
                            , matchesString
                            )
import Yi.Keymap.Vim.Utils  ( mkStringBindingE, mkStringBindingY )

v2KeymapSet :: ((EventString -> EditorM ()) -> [VimBinding]) -> KeymapSet
v2KeymapSet myBindings = mkKeymapSet $ defVimConfig `override` \super this ->
    let eval = pureEval this
    in super { vimBindings = myBindings eval ++ vimBindings super }

class NMap x y where
    nmap :: x -> y -> VimBinding

instance NMap (EventString) (BufferM ()) where
    nmap x y = nmap x (withBuffer0 y)

instance NMap (EventString) (EditorM ()) where
    nmap x y = mkStringBindingE Normal Drop (x, y, id)

instance NMap (EventString) (YiM ()) where
    nmap x y = mkStringBindingY Normal (x, y, id)

class IMap x y where
    imap :: x -> y -> VimBinding

instance IMap (EventString) (BufferM ()) where
    imap x y = imap x (withBuffer0 y)

instance IMap (EventString) (EditorM ()) where
    imap x y = VimBindingE (\evs state -> case vsMode state of
                              Insert _ ->
                                  fmap (const (y >> return Continue))
                                       (evs `matchesString` x)
                              _ -> NoMatch)

leader :: EventString -> EventString
leader = mappend "\\"

key :: EventString -> EventString
key = id
