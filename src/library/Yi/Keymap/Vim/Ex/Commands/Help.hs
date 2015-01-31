{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Vim.Ex.Commands.Yi
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable


module Yi.Keymap.Vim.Ex.Commands.Help (parse) where

import Control.Monad ( void )
import Control.Applicative ( (<$>) )
import Yi.Command.Help ( displayHelpFor )
import qualified Data.Text as T ( pack, append )
import qualified Text.ParserCombinators.Parsec as P
    ( try, option, many1, string, space, anyChar )
import Yi.Keymap ( Action(YiA) )
import Yi.Keymap.Vim.Common ( EventString )
import qualified Yi.Keymap.Vim.Ex.Commands.Common as Common
    ( parse, impureExCommand )
import Yi.Keymap.Vim.Ex.Types ( ExCommand(cmdAction, cmdShow) )

parse :: EventString -> Maybe ExCommand
parse = Common.parse $ do
    void $ P.string "help"
    cmd <- P.option "" $ P.try $ do
      void $ P.many1 P.space
      T.pack <$> P.many1 P.anyChar
    return $! Common.impureExCommand {
        cmdAction   = YiA $ displayHelpFor cmd
      , cmdShow     = "help" `T.append`
                      if cmd == ""
                         then ""
                         else " " `T.append` cmd
      }
