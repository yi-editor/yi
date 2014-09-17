{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.GotoLine
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Keymap.Vim.Ex.Commands.GotoLine (parse) where

import           Data.Char (isDigit)
import qualified Data.Text as T
import           Yi.Buffer
import           Yi.Keymap
import           Yi.Keymap.Vim.Common
import           Yi.Keymap.Vim.Ex.Commands.Common (pureExCommand)
import           Yi.Keymap.Vim.Ex.Types

parse :: EventString -> Maybe ExCommand
parse (Ev s) = if not (T.null s) && T.all isDigit s
    then let l = read $ T.unpack s in
         Just $ pureExCommand {
             cmdAction = BufferA $ gotoLn l >> firstNonSpaceB
           , cmdShow = s
         }
    else Nothing
