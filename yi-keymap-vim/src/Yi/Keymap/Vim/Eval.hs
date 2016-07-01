{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Eval
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module doesn't contains actual eval, see
-- 'Yi.Keymap.Vim.vimEval' comment.

module Yi.Keymap.Vim.Eval (scheduleActionStringForEval) where

import Yi.Editor                (EditorM)
import Yi.Keymap.Vim.Common     (EventString, VimState (vsStringToEval))
import Yi.Keymap.Vim.StateUtils (modifyStateE)

scheduleActionStringForEval :: EventString -> EditorM ()
scheduleActionStringForEval s = modifyStateE $ \st -> st { vsStringToEval = s }
