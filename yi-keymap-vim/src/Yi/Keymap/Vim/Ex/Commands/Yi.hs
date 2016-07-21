{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Yi
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable


module Yi.Keymap.Vim.Ex.Commands.Yi (parse) where

import           Control.Monad                    (void)
import qualified Data.Attoparsec.Text             as P (many1, space, string, takeText)
import qualified Data.Text                        as T (unpack)
import           Yi.Eval                          (execEditorAction)
import           Yi.Keymap                        (Action (YiA))
import           Yi.Keymap.Vim.Common             (EventString)
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common (impureExCommand, parse)
import           Yi.Keymap.Vim.Ex.Types           (ExCommand (cmdAction, cmdShow))

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "yi"
    void $ P.many1 P.space
    cmd <- P.takeText
    return $! Common.impureExCommand {
        cmdAction = YiA $ execEditorAction (T.unpack cmd)
      , cmdShow = cmd
      }
