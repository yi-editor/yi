{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Yi
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable


module Yi.Keymap.Vim.Ex.Commands.Yi (parse) where

import           Control.Monad
import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import           Yi.Eval (execEditorAction)
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "yi"
    void $ P.many1 P.space
    cmd <- P.many1 P.anyChar
    return $! Common.impureExCommand {
        cmdAction = YiA $ execEditorAction cmd
      , cmdShow = T.pack cmd
      }
